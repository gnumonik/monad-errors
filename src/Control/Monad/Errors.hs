{-# LANGUAGE QuantifiedConstraints, GeneralisedNewtypeDeriving, DeriveFunctor #-}
{- |
-- Module      :  Control.Monad.Errors
-- Copyright   :  (c) Gregg S. Hunter 2021 
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  gnumonik@protonmail.com
-- Stability   :  experimental
--
-- An Error handling monad which collects all errors instead of short-circuiting. 
-----------------------------------------------------------------------------
-}

module Control.Monad.Errors where 

import Control.Monad.Trans.Class ( MonadTrans(..) )
import Data.Functor.Compose ( Compose(..) ) 
import Control.Applicative.Lift
    ( failure, runErrors, Errors, Lift(Pure, Other), eitherToErrors ) 
import Control.Monad.Identity
    ( void, Identity(runIdentity), IdentityT (runIdentityT), (<$!>) )
import Data.Kind 
import Control.Monad.Trans.State.Strict 
import Control.Monad.Codensity 
import Control.Monad.Errors.Class 
import Data.Functor.Const
import Data.Functor.Constant
import qualified Control.Foldl as F 
import Data.Foldable (Foldable(toList))

newtype ErrorsT e m a = ErrorsT {unE :: Codensity (m :.: ErrorsK e) (ErrorsK e a)}
  deriving Functor

type CompE e m a = (m :.: Errors e) a

type ErrorsI e a = ErrorsT e Identity a 

instance (Monoid e ) => MonadTrans (ErrorsT e) where 
  lift m = ErrorsT $ Codensity $ (go m)
    where 
      go :: forall m a. Monad m => m a -> forall b. (ErrorsK e a -> Compose m (ErrorsK e) b) -> Compose m (ErrorsK e) b
      go m' f =  Compose $ do 
          m'' <- m'
          getCompose . f . pure $ m''

instance (Monoid e, Monad m) => Applicative (ErrorsT e m) where 
  pure x = ErrorsT . pure . pure $! x   

  (ErrorsT f) <*> (ErrorsT  !x) 
    = ErrorsT $ (fmap go f) <*> x 
   where 
     go :: forall e a b. Monoid e => ErrorsK e (a -> b) -> ErrorsK e a -> ErrorsK e b
     go g h = (g <*> h)
  {-# INLINE (<*>) #-}

instance (Monoid e, Monad m) => Monad (ErrorsT  e m) where 
  return = pure 

  (ErrorsT !ma) >>= f = ErrorsT $! runErrorsK <$!> ma >>= \case 
    Left err -> pure . failK $! err 
    Right a  -> unE $! f a  
  {-# INLINE (>>=) #-}


instance (Monoid e, MonadTrans (ErrorsT e)) => MonadErrors e (ErrorsT e ) where 
  runE (ErrorsT m) = (getCompose . lowerCodensity $ m) >>= \x -> case runErrorsK x of 
    Left e -> pure . Left $ e 
    Right e -> pure $ runErrorsK e 

  compE ma = ErrorsT $ Codensity (go . Compose $ ma)
    where 
      go :: forall  x m. Monad m => Compose m (ErrorsK e) x -> forall b. (ErrorsK e x -> Compose m (ErrorsK e) b) -> Compose m (ErrorsK e) b
      go m f = Compose $  do 
        m' <- getCompose m 
        getCompose $ f m' 
  {-# INLINE compE #-} 

collecting :: forall h e t a m b.  (Traversable h, MonadErrors e t, Monad m) => (a -> t m b) -> h a -> m (Either e (h b))
collecting f t = runE . traverse f $ t  


collect_ :: forall h e t m a b. (Traversable h, MonadErrors e t, Monad m) => (a -> t m b) -> h a ->  m ()
collect_ f t =  void . runE . traverse f $ t  



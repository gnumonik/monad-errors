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
import Control.Monad.Codensity
    ( Codensity(Codensity), lowerCodensity ) 
import Control.Monad.Errors.Class
    ( type (:.:), MonadErrors(report, unliftE, runE, catchE, compE) )
import Control.Monad.Errors.LiftK ( ErrorsK, runErrorsK, failK )
import qualified Control.Foldl as F 
import Data.Foldable (Foldable(toList, foldl'))
import Control.Monad.State.Class ( MonadState(put, get) ) 
import Control.Monad.IO.Class ( MonadIO(..) ) 
import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad ( MonadPlus, (<$!>) )

newtype ErrorsT e m a = ErrorsT {unE :: Codensity (m :.: ErrorsK e) (ErrorsK e a)}
  deriving Functor

type CompE e m a = (m :.: Errors e) a

type ErrorsI e a = ErrorsT e Identity a 

instance MonadTrans (ErrorsT e) where 
  lift m = ErrorsT $ Codensity $ (go m)
    where 
      go :: forall m a. Monad m => m a -> forall b. (ErrorsK e a -> Compose m (ErrorsK e) b) -> Compose m (ErrorsK e) b
      go m' f =  Compose $ do 
          m'' <- m'
          getCompose . f . pure $ m''

instance (Applicative m) => Applicative (ErrorsT e m) where 
  pure x = ErrorsT . pure . pure $! x 

  (ErrorsT f) <*> (ErrorsT  !x) 
    = ErrorsT $ fmap go f <*> x 
   where 
     go :: forall e a b. ErrorsK e (a -> b) -> ErrorsK e a -> ErrorsK e b
     go g !h = g <*> h
  {-# INLINE (<*>) #-}

instance (Monoid e, Monad m) => Monad (ErrorsT  e m) where 
  return = pure 

  (ErrorsT !ma) >>= f = ErrorsT $! runErrorsK <$!> ma >>= \case 
    Left !err -> pure . failK $! err 
    Right !a  -> unE $! f a  
  {-# INLINE (>>=) #-}


instance (Monoid e) => MonadErrors e (ErrorsT e ) where 
  runE (ErrorsT m) = (getCompose . lowerCodensity $ m) >>= \x -> case runErrorsK x of 
    Left e -> pure . Left $ e 
    Right e -> pure $ runErrorsK e 

  compE ma = ErrorsT $ Codensity (go . Compose $ ma)
    where 
      go :: forall  x m. Monad m => Compose m (ErrorsK e) x -> forall b. (ErrorsK e x -> Compose m (ErrorsK e) b) -> Compose m (ErrorsK e) b
      go m f = Compose $  do 
        m' <- getCompose m 
        getCompose $ f m' 

  catchE ey xy (ErrorsT m) = ErrorsT $! runErrorsK <$!> m >>= \case
    Left err -> pure . pure $ ey err 
    Right a  -> pure . pure $ xy a  
  {-# INLINE compE #-} 

instance (MonadIO (t m), Monoid e) => MonadIO (ErrorsT e (t m)) where 
  liftIO = unliftE . liftIO 


instance (Monoid e, Monad m)=> Alternative (ErrorsT e m) where 
  empty = report mempty 

  (ErrorsT ma) <|> (ErrorsT mb) = ErrorsT $! runErrorsK <$!> ma >>= \case 
    Left err ->  mb 
    Right a  -> ma 

instance (Monoid e, Monad m) => MonadPlus (ErrorsT e m)

instance (Monoid e, Monad m) => MonadFail (ErrorsT e m) where 
  fail _ = report mempty 

instance (MonadState s m, Monoid e) => MonadState s (ErrorsT e m) where 
  get = unliftE get 

  put = unliftE . put  


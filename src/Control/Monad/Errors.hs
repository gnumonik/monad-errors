{-# LANGUAGE QuantifiedConstraints #-}
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
    ( failure, runErrors, Errors, Lift(Pure) ) 
import Control.Monad.Identity
    ( void, Identity(runIdentity), IdentityT (runIdentityT) )
import Data.Kind 
import Control.Monad.Trans.State.Strict 

import Control.Monad.Errors.Class 

newtype ErrorsT e m a = ErrorsT {unE :: (m :.: Errors e) a}

-- | Type synonym for when we don't care about the inner monad.
type ErrorsI e a = ErrorsT e Identity a 

runEM :: MonadErrors e (ErrorsT e )  => ErrorsI e a -> (Either e a)
runEM (ErrorsT e) = runErrors . runIdentity . getCompose $ e 

-- | Type synonym for Data.Functor.Compose.Compose. Used solely to make signatures more readable. 
--   (If anyone knows of a way to make GHC report type errors involving type operators 
--    in an infix format, please let me know)
type f :.: g = Compose f g 
-- todo: figure out the right fixity

-- | `ErrorsT e` is a monad transformer for every monad `m` for which `ErrorsT e m` is a monad
instance (forall m. Monad m => Monad (ErrorsT e m)) => MonadTrans (ErrorsT e) where 
  lift m = ErrorsT . Compose $ m >>= \x -> pure . Pure $ x 

-- | `ErrorsT t e m` is a Functor if `t m` is a functor
instance (Functor t) => Functor (ErrorsT e t) where 
  fmap f (ErrorsT o) = ErrorsT . Compose . (fmap . fmap) f . getCompose $ o

instance (Monoid e, Monad m) => Applicative (ErrorsT e m) where 
  pure x = ErrorsT . Compose . pure . pure $ x  

  (ErrorsT f) <*> (ErrorsT x) = ErrorsT . Compose $ 
  --hlint yells at me if i use lambda case and i can't figure out how to turn it off so...
    getCompose  f >>= \w -> case runErrors w of  
      Left e -> getCompose x >>= \x' -> 
        case runErrors x' of  
          Left e' -> pure $ failure (e <> e')
          Right _ -> pure $ failure e
      Right f' -> getCompose x >>= \x' -> 
        case runErrors x' of   
          Left e -> pure $ failure e  
          Right a' -> pure . Pure $  f' a'  

instance (Monoid e, Monad m) => Monad (ErrorsT  e m) where 
  return = pure 

  (ErrorsT ma) >>= f = ErrorsT . Compose $ 
    getCompose ma >>= \x -> case runErrors x of 
      Left err -> pure . failure $ err 
      Right a  -> case f a of 
        ErrorsT mb -> getCompose mb >>= \x -> 
          case runErrors x of   
            Left err -> pure . failure $ err 
            Right b  -> pure . Pure $ b           

instance (Monoid e, forall m. Monad m => Monad (ErrorsT e m)) => MonadErrors e (ErrorsT e ) where 
  decompE (ErrorsT e) = let bloop =  getCompose e   
                        in bloop 
  compE t = ErrorsT . Compose $ t  

-- | Convenience function. 
--   collect f t = runE $ traverse f t 
collecting :: (Traversable h, MonadErrors e t, Monad m) => (a -> t m b) -> h a -> m (Either e (h b))
collecting f t = runE $ traverse f t

-- | Convenience function. 
--   collect f t = runE $ traverse f t 
collect_ :: forall h e t m a b. (Traversable h, MonadErrors e t, Monad m) => (a -> t m b) -> h a ->  m ()
collect_ f t =  void . runE $ traverse f t
-- collect_ requires a helper function without the injectivity annotation on MonadErrors 



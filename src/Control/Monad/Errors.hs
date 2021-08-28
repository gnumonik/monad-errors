{- |
-- Module      :  Control.Monad.Errors
-- Copyright   :  (c) Gregg S. Hunter 2021 
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  gnumonik@protonmail.com
-- Stability   :  experimental
--
-- An Error handling monad that collects all errors instead of short-circuiting. 
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

-- | Type variable interpretation key: 
--   `t` stands for some Monad Transformer, i.e., something of kind (Type -> Type) -> Type -> Type) which, when applied to a monad, yields a monad 
--   `e` stands for the error type and *must be a Monoid* 
--   `m` stands for the inner monad which, when `t` is applied to it, yields a monad 
--   `a` stands for the result type 

-- | ErrorsT t e m a is a newtype wrapper around the composition 
--   of a monad transformer stack `t m` and `Errors e a` from 
--   Control.Applicative.Lift 
newtype ErrorsT t e m a = ErrorsT {unE :: (t m :.: Errors e) a}

-- | Type synonym for when we don't care about the inner monad.
type ErrorsMT t e a = ErrorsT t e Identity a 

-- | Type synonym for when we're just using a normal, non-transformer monad 
type ErrorsM e m a = ErrorsT IdentityT e m a 

runEM :: MonadErrors ErrorsT IdentityT e m  => ErrorsM e m a -> m (Either e a)
runEM em = runIdentityT $ runE em 

-- | Type synonym for when we just want the error collection
type ErrorsI e a = ErrorsT IdentityT e Identity a 

runEI :: MonadErrors ErrorsT IdentityT e Identity 
      => ErrorsI e a -> Either e a 
runEI = runIdentity . runIdentityT . runE 

-- | Type synonym for Data.Functor.Compose.Compose. Used solely to make signatures more readable. 
--   (If anyone knows of a way to make GHC report type errors involving type operators 
--    in an infix format, please let me know)
type f :.: g = Compose f g 
-- todo: figure out the right fixity

-- | `ErrorsT t e` is a monad transformer if `t` is 
instance (MonadTrans t, Monoid e) => MonadTrans (ErrorsT t e) where 
  lift m = ErrorsT . Compose . lift $ pure <$> m 

-- | `ErrorsT t e m` is a Functor if `t m` is a functor
instance (Functor (t m)) => Functor (ErrorsT t e m) where 
  fmap f (ErrorsT o) = ErrorsT . Compose . (fmap . fmap) f . getCompose $ o

instance (Monoid e, Monad m, Monad (t m)) => Applicative (ErrorsT t e m) where 
  pure x = ErrorsT . Compose . pure . pure $ x  

  (ErrorsT f) <*> (ErrorsT x) = ErrorsT . Compose $ 
    getCompose  f >>= \w -> case runErrors w of  
      Left e -> getCompose x >>= \x' -> 
        case runErrors x' of  
          Left e' -> pure $ failure (e <> e')
          Right _ -> pure $ failure e
      Right f' -> getCompose x >>= \x' -> 
        case runErrors x' of   
          Left e -> pure $ failure e  
          Right a' -> pure . Pure $  f' a'  

instance (Monoid e, Monad m, Monad (t m)) => Monad (ErrorsT t e m) where 
  return = pure 

  (ErrorsT ma) >>= f = ErrorsT . Compose $ 
    getCompose ma >>= \x -> case runErrors x of 
      Left err -> pure . failure $ err 
      Right a  -> case f a of 
        ErrorsT mb -> getCompose mb >>= \x -> 
          case runErrors x of   
            Left err -> pure . failure $ err 
            Right b  -> pure . Pure $ b           

instance (Monoid e, Monad m,  Monad (t m)) => MonadErrors ErrorsT t e m where 
  decompE e = getCompose . unE $ e  

  compE t = ErrorsT . Compose $ t  

-- | Convenience function. 
--   collect f t = runE $ traverse f t 
collect :: (Traversable h, MonadErrors f t e m) => (a -> f t e m b) -> h a -> t m (Either e (h b))
collect f t = runE $ traverse f t
-- | Convenience function. 
--   collect f t = runE $ traverse f t 
collect_ :: (Traversable h, MonadErrors f t e m) => (a -> f t e m b) -> h a -> t m ()
collect_ f t = void . runE $ traverse f t 

type TestOops = ErrorsT (StateT Int) [(Int,String)] Identity 

countEven :: Int -> TestOops Int 
countEven n = if odd n 
              then throwE [(n,"not even")] 
              else do 
                asT $ modify (+1)
                asT get >>= pure 

errorsTest :: (Either [(Int, String)] [Int], Int)
errorsTest = runIdentity $ runStateT (collect countEven [1..10]) 0

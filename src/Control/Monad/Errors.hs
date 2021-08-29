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
    ( failure, runErrors, Errors, Lift(Pure, Other), eitherToErrors ) 
import Control.Monad.Identity
    ( void, Identity(runIdentity), IdentityT (runIdentityT) )
import Data.Kind 
import Control.Monad.Trans.State.Strict 
import Control.Monad.Codensity 
import Control.Monad.Errors.Class 
import Data.Functor.Const
import Data.Functor.Constant

newtype ErrorsT e m a = ErrorsT {unE :: Codensity (m :.: ErrorsK e) (Errors e a)}

squish :: Monad (ErrorsT e m) => ErrorsT e m (Errors e a) -> ErrorsT e m a 
squish (ErrorsT e) = ErrorsT $ e >>= \case 
    Other (Constant e) -> pure $ failure e
    Pure a  -> case a of 
      Other (Constant e) -> pure $ failure e
      Pure a -> pure . Pure $ a 

type CompE e m a = (m :.: Errors e) a

-- | Type synonym for when we don't care about the inner monad.
type ErrorsI e a = ErrorsT e Identity a 

--runEM :: MonadErrors e (ErrorsT e )  => ErrorsI e a -> (Either e a)
--runEM (ErrorsT e) = runErrors . runIdentity . getCompose $ e 


-- todo: figure out the right fixity

-- | `ErrorsT e` is a monad transformer for every monad `m` for which `ErrorsT e m` is a monad
instance (Monoid e ) => MonadTrans (ErrorsT e) where 
  lift m = ErrorsT $ Codensity $ (go m)
    where 
      go :: forall m a. Monad m => m a -> forall b. (Errors e a -> Compose m (ErrorsK e) b) -> Compose m (ErrorsK e) b
      go m' f =  Compose $ do 
          m'' <- m'
          getCompose . f . Pure $ m''
 
    -- (boop :: m :.: Errors e) <- pure $ Compose $ m >>= \x -> pure . Pure $ x 
   -- d
  
  {-- \f -> do 
    let   g :: forall t m a. (Monad m, Monad (m :.: Errors e))=> m a -> (m :.: Errors e) a  
          g y =  Compose $ y >>= \x -> pure . Pure $ x 
    (e :: Either e a) <- lift . runE $ g m
    f e 
  --}

-- | `ErrorsT t e m` is a Functor if `t m` is a functor
instance (Functor t) => Functor (ErrorsT e t) where 
  fmap f (ErrorsT o) = ErrorsT $ (fmap . fmap) f o
  {-# INLINE fmap #-}

instance (Monoid e, Monad m) => Applicative (ErrorsT e m) where 
  pure x = ErrorsT . pure . pure $ x   

  (ErrorsT f) <*> (ErrorsT  x) 
    = ErrorsT $ (fmap go f) <*> x 
   where 
     go :: forall e a b. Monoid e => Errors e (a -> b) -> Errors e a -> Errors e b
     go !g !h = (g <*> h)
     {-- ErrorsT $ do 
   where 
      f' <-  f 
      x' <-  x 
      pure $ (f' <*> x') --}
  {-# INLINE (<*>) #-}
     {-- Codensity $ \k -> 
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
--}
instance (Monoid e, Monad m) => Monad (ErrorsT  e m) where 
  return = pure 

  (ErrorsT !ma) >>= f = ErrorsT $! ma >>= \case 
    Other !(Constant !e) -> pure $! failure e
    Pure !a  -> let (ErrorsT fa) = f a 
                in fa  
  {-# INLINE (>>=) #-}
    {-- 
    
    . Compose $ 
    getCompose ma >>= \x -> case runErrors x of 
      Left err -> pure . failure $ err 
      Right a  -> case f a of 
        ErrorsT mb -> getCompose mb >>= \x -> 
          case runErrors x of   
            Left err -> pure . failure $ err 
            Right b  -> pure . Pure $ b           
--}


instance (Monoid e, MonadTrans (ErrorsT e)) => MonadErrors e (ErrorsT e ) where 
  runE (ErrorsT m) = (getCompose . lowerCodensity $ m) >>= \x -> case runErrorsK x of 
    Left e -> pure . Left $ e 
    Right e -> pure $ runErrors e 
  --decompE (ErrorsT e) = 
    
    {--
    go $! getCompose (lowerCodensity e)   
    where 
      go ::forall m  x. Monad m =>  m (Errors e (Errors e x)) ->  m (Errors e x) 
      go !ma  = ma >>= \ !x -> \case 
        Left !err -> pure $ failure err
        Right !a  -> case a of 
          Left !err -> pure $ failure err 
          Right !a' -> pure . pure $! a'
  {-# INLINE decompE #-}  
  --}

  compE ma = ErrorsT $ Codensity (go . Compose $ ma)
    where 
      go :: forall  x m. Monad m => Compose m (ErrorsK e) x -> forall b. (Errors e x -> Compose m (ErrorsK e) b) -> Compose m (ErrorsK e) b
      go m f = Compose $  do 
        (ErrorsK m') <- getCompose m 
        getCompose . f $ toLift m' 
  {-# INLINE compE #-} 



-- | Convenience function. 
--   collect f t = runE $ traverse f t 
collecting :: (Traversable h, MonadErrors e t, Monad m) => (a -> t m b) -> h a -> m (Either e (h b))
collecting f t = runE . sequenceA . fmap f $ t  
{-# INLINE collecting #-}

-- | Convenience function. 
--   collect f t = runE $ traverse f t 
collect_ :: forall h e t m a b. (Traversable h, MonadErrors e t, Monad m) => (a -> t m b) -> h a ->  m ()
collect_ f t =  void . runE . sequenceA . fmap f $ t  
-- collect_ requires a helper function without the injectivity annotation on MonadErrors 



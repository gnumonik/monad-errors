{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
-- Module      :  Control.Monad.Errors
-- Copyright   :  (c) Gregg S. Hunter 2021 
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  gnumonik@protonmail.com
-- Stability   :  experimental
--
-- A typeclass for error handling monads which collects all errors instead of short-circuiting. 
-----------------------------------------------------------------------------
-}

module Control.Monad.Errors.Class  where

import Control.Applicative.Lift
    ( failure, runErrors, Errors, Lift(Pure), unLift )
import Data.Kind (Type, Constraint)
import Control.Applicative (liftA2, Const (Const))
import Control.Monad.Trans.Class
import Data.Constraint.Forall
import Data.Functor.Compose
import Control.Monad.Codensity
import Data.Functor.Constant

-- | Type synonym for Data.Functor.Compose.Compose. Used solely to make signatures more readable. 
--   (If anyone knows of a way to make GHC report type errors involving type operators 
--    in an infix format, please let me know)
type f :.: g = Compose f g 

class (MonadTrans t, forall m. Monad m => Monad (t m)) => MonadErrors e t | t -> e where

  --decompE :: forall m x. Monad m => t m x ->  m (Errors e x)

  compE :: forall m x. Monad m => m (ErrorsK e x) -> t m x 

  report  :: forall m x. Monad m => e -> t m x 
  report = liftE . Left 

  runE :: forall m x. Monad m => t m x  -> m (Either e x)


  liftE   :: forall m x. Monad m => Either e x -> (t m) x  
  liftE = \case 
    Left err -> compE . pure $ failK err
    Right a  -> pure a 

type Return r f a = f a -> r 



-- either :: forall x. (l -> x) -> (r -> x) -> x 
-- lift   :: forall x. Applicative f => (l -> f x) -> (r -> x) -> x 

type LiftK f a = Codensity (Lift f) a 

unLiftK :: Applicative f => LiftK f a -> f a 
unLiftK lk = unLift . lowerCodensity $ lk 

toLift :: Applicative f => LiftK f a -> Lift f a 
toLift lk = lowerCodensity lk 

newtype ErrorsK e a = ErrorsK (LiftK (Constant e) a) deriving (Functor, Applicative, Monad) 

runErrorsK :: Monoid e => ErrorsK e a -> Either e a 
runErrorsK (ErrorsK f) = runErrors . toLift $ f  

failK :: e -> ErrorsK e a
failK e = ErrorsK $ Codensity $ \_ -> failure e  

eitherToErrorsK :: Monoid e => Either e a -> ErrorsK e a 
eitherToErrorsK = \case 
  Left err -> failK err 
  Right a  -> pure a 


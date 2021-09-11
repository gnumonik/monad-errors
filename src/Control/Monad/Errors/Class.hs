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
import Control.Monad.Trans.Class ( MonadTrans )
import Data.Constraint.Forall ()
import Data.Functor.Compose ( Compose )
import Control.Monad.Codensity
    ( Codensity(Codensity), lowerCodensity )
import Data.Functor.Constant ( Constant )
import Control.Monad.Errors.LiftK
    ( ErrorsK, failK, eitherToErrorsK )


-- | Type synonym for Data.Functor.Compose.Compose. Used solely to make signatures more readable. 
--   (If anyone knows of a way to make GHC report type errors involving type operators 
--    in an infix format, please let me know)
type f :.: g = Compose f g 

class (MonadTrans t, forall m. Monad m => Monad (t m), Monoid e) => MonadErrors e t | t -> e where

  compE :: forall m x. Monad m => m (ErrorsK e x) -> t m x 

  report  :: forall m x. Monad m => e -> t m x 
  report = liftE . Left 

  runE :: forall m x. Monad m => t m x  -> m (Either e x)

  liftE   :: forall m x. Monad m => Either e x -> (t m) x  
  liftE = \case 
    Left !err -> compE . pure $! failK err
    Right !a  -> pure a 

  catchE :: forall m x y. (e -> y) -> (x -> y) -> t m x -> t m y  

-- this really shouldn't be named unliftE
  unliftE :: forall m x. Monad m => m x -> t m x 
  unliftE mx = compE $  fmap (eitherToErrorsK @e . Right) mx 





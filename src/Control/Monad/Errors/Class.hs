{-# LANGUAGE QuantifiedConstraints #-}
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

module Control.Monad.Errors.Class (MonadErrors(..))where

import Control.Applicative.Lift 
    ( failure, runErrors, Errors, Lift(Pure) )
import Data.Kind (Type, Constraint)
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class

class (MonadTrans t, forall m. Monad m => Monad (t m)) => MonadErrors e t | t -> e where

  decompE :: forall m x. Monad m => t m x ->  m (Errors e x)

  compE :: forall m x. Monad m => m (Errors e x) -> t m x 

  report  :: forall m x. Monad m => e -> t m x 
  report = liftE . Left 

  runE :: forall m x. Monad m => t m x  -> m (Either e x)
  runE m = runErrors <$> decompE m 

  liftE   :: forall m x. Monad m => Either e x -> (t m) x  
  liftE = \case 
    Left err -> compE . pure $ failure err
    Right a  -> pure a 








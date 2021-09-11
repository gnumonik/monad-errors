{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
-- Module      :  Control.Monad.Errors
-- Copyright   :  (c) Gregg S. Hunter 2021 
-- License     :  MIT (see LICENSE file)
--
-- Maintainer  :  gnumonik@protonmail.com
-- Stability   :  experimental
--
-- A CPS-ized version of Control.Applicative.Lift 
-----------------------------------------------------------------------------
-}
module Control.Monad.Errors.LiftK where

import Control.Monad.Codensity
    ( lowerCodensity, Codensity(Codensity) )
import Control.Applicative.Lift
    ( Lift, runErrors, failure, unLift )
import Data.Functor.Constant ( Constant )

type LiftK f a = Codensity (Lift f) a 

unLiftK :: Applicative f => LiftK f a -> f a 
unLiftK lk = unLift . lowerCodensity $ lk 

toLift :: Applicative f => LiftK f a -> Lift f a 
toLift lk = lowerCodensity lk 

newtype ErrorsK e a = ErrorsK (LiftK (Constant e) a) deriving (Functor, Applicative, Monad) 

runErrorsK :: Monoid e => ErrorsK e a -> Either e a 
runErrorsK (ErrorsK !f) = runErrors . toLift $ f  

failK :: e -> ErrorsK e a
failK e = ErrorsK $! Codensity $ \ !_ -> failure e  

eitherToErrorsK :: forall e a. Monoid e => Either e a -> ErrorsK e a 
eitherToErrorsK = \case 
  Left !err -> failK err 
  Right !a  -> pure a 

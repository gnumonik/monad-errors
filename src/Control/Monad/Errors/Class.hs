module Control.Monad.Errors.Class (MonadErrors(..))where

import Control.Applicative.Lift as L
    ( failure, runErrors, Errors, Lift(Pure) )
import Data.Kind (Type)


-- | MonadErrors class. 
--   Type variable interpretation key: 
--   `f` is a type constructor which takes a monad transformer `t`
--   , an error type `e` (which must be a Monoid)
--   , an inner monad `m`
--   , and an argument type.
--   `t` doesn't *have* to be an instance of MonadTrans, although `t m` must be a monad.
--   `f t e m` must be a monad. 
--   It may be useful to think of this as a class for a specific kind of "Monad Transformer Transformer" 
--   insofar as it adds error collection functionaliy to a given monad transformer. 
--   This class is primarily intended for use with the ErrorsT newtype.
--   (There is probably a more succinct way to define this class using Forall from the constraints package 
--   and/or the QuantifiedConstraints extension.) 
class ( Monoid e
      , Monad (t m)
      , Monad (f t e m)) => MonadErrors f t e m where

  -- | `asT` lifts a monadic computation in the "argument transformer stack" to the MonadErrors monad 
  -- 
  -- > compE $ mx >>= \x -> pure . Pure $ x
  asT :: forall x. t m x -> f t e m x 
  asT mx = compE $ mx >>= \x -> pure . Pure $ x 

  -- | `asE` lifts an (Errors e x) applicative computation to the MonadErrors monad 
  -- 
  -- > asE = liftE . runE  
  asE  :: forall x. Errors e x -> f t e m x 
  asE = liftE . runErrors

  -- | `decompE` decomposes the MonadErrors monad into its "argument transformer stack"
  -- 
  --   For example, if `f` is a newtype which wraps the composition of a monad transformer stack `t m` and 
  --   `Errors e x` such as:
  --  
  -- > newtype F x = F {unF :: (ReaderT Int IO :.: Errors [String] x}
  --  
  --   Then decompE can be defined as:
  -- 
  -- > decompE = getCompose . unF 
  decompE :: forall x. f t e m x -> t m (Errors e x)

  -- | `compE` wraps a an `Errors e x` compution in the "argument transformer stack" 
  --   up into the MonadErrors monad.
  -- 
  --   For example, if `f` is a newtype which wraps the composition of a monad transformer stack `t m` and 
  --   `Errors e x`, such as: 
  --
  -- > newtype F x = F {unF :: (ReaderT Int IO :.: Errors [String] x} 
  --
  --   Then compE can be defined as:
  -- 
  -- > compE = F . Compose
  compE :: forall x. t m (Errors e x) -> f t e m x 

  -- | `throwE` lifts an Error value `e` (which must be a monoid) into the MonadErrors monad  
  --
  -- > throwE = liftE . Left
  throwE  :: forall x. e -> f t e m x 
  throwE = liftE . Left 

  -- | `runE` extracts an `Either e x` value (wrapped in the "argument transformer stack")
  --   from the MonadErrors monad. 
  --
  -- > runE m = runE <$> decompE m
  runE :: forall x. f t e m x  -> t m (Either e x)
  runE m = runErrors <$> decompE m 

  -- | `liftE` lifts an `Either e x` value into the MonadErrors monad 
  liftE   :: forall x. Either e x -> f t e m x  
  liftE = \case 
    Left err -> compE . pure $ failure err
    Right a  -> pure a 

  -- | `unliftE` "peeks" the current "result" in the MonadErrors monad 
  unliftE :: forall x. f t e m x -> f t e m (Either e x)
  unliftE m = do 
    let m' = decompE m 
    x <- asT m' 
    liftE . pure $ runErrors x 
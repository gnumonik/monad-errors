import Control.Monad.Errors  
import Control.Monad.Errors.Class

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity 
import Control.Monad.Trans.Class 
import System.TimeIt
import Data.Monoid (Sum(..))
import Control.Monad (replicateM)

main :: IO ()
main = runTest1

type TestStateT = ErrorsT (Sum Int)  (StateT Int Identity)

countEven :: Int -> TestStateT Int 
countEven !n = if odd n 
              then report (Sum n)
              else do 
                lift $! modify (+1)
                s <- lift get 
                pure s 

test1 :: Int -> IO () -- (Either [(Int, String)] [Int], Int)
test1 n = do 
  a <- pure . runIdentity $ runStateT (runE $ traverse countEven $  [0..n]) 0
  print . snd $ a 
 

runTest1 :: IO ()
runTest1 = do 
  let f x = timeItNamed (show x) (test1 x)
  f 1000 
  f 10000
  f 100000
  f 1000000
  f 10000000
  f 100000000
  f 1000000000



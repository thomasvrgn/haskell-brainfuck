module Core.Interpreter where
  import Data.Functor.Identity
  import Control.Monad.State

  import Types.Element
  import Types.Stack

  eval :: Program -> StateStack IO ()
  eval (Increment:xs) = updateAddressValue Increment >> eval xs
  eval (Output:xs) = output
module Core.Interpreter where
  import Data.Functor.Identity
  import Control.Monad.State

  import Types.Element
  import Types.Stack

  eval :: Program -> StateStack IO ()
  eval [] = return ()
  eval (Increment:xs) = updateAddressValue Increment >> eval xs
  eval (Decrement:xs) = updateAddressValue Decrement >> eval xs
  eval (Input:xs) = input >> eval xs
  eval (Output:xs) = output >> eval xs
  eval (Next:xs) = movePointer Next >> eval xs
  eval (Previous:xs) = movePointer Previous >> eval xs
  eval z@(Loop a:xs) = do
    cell <- getCurrentCell
    if cell == 0 then eval xs else eval a >> eval z
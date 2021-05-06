module Types.Stack where
  import Control.Monad.State
  import Data.Functor.Identity
  import Data.Char
  import Types.Element

  type Stack = [Integer] -- Stack only contains bytes
  type Pointer = Int -- Pointer is just kind of stack index
  
  type StateStack m a = StateT (Stack, Pointer) m a

  uppercase :: String -> String
  uppercase = map toUpper

  replaceAtIndex :: [a] -> a -> Int -> [a]
  replaceAtIndex list x i = map (\(i', x') -> if i' == i then x else x') (zip [0..] list)

  updateAddressValue :: Element -> StateStack IO ()
  updateAddressValue action = do
    (stack, ptr) <- get
    let el = stack !! ptr
    let new = (replaceAtIndex stack (case action of 
          Increment -> el + 1
          Decrement -> el - 1
          _ -> el) ptr)
    put (new, ptr)

  movePointer :: Element -> StateStack IO ()
  movePointer action = do
    (stack, ptr) <- get
    put (stack, case action of
      Next -> if ptr + 1 >= length stack then 0 else ptr + 1
      Previous -> if ptr - 1 < 0 then (length stack) - 1 else ptr - 1
      _ -> ptr)

  getCurrentCell :: StateStack IO Integer
  getCurrentCell = do
    (stack, ptr) <- get
    return (stack !! ptr)

  output :: StateStack IO ()
  output = do
    (stack, ptr) <- get
    let byte = stack !! ptr
    if byte < 0 then fail "Can't output negative char !" else
      liftIO . putChar . chr . fromInteger $ byte
  
  input :: StateStack IO ()
  input = do
    char <- liftIO $ ord <$> getChar
    (stack, ptr) <- get
    let new = replaceAtIndex stack (toInteger char) ptr
    put (new, ptr)

  type Size = Int
  initStack :: Size -> Stack
  initStack x = map (\x -> 0) [0..x]
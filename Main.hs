module Main where
  import Core.Parser
  import Core.Interpreter

  import Types.Element
  import Types.Stack

  import Text.Parsec.Error
  import Data.List
  import System.Directory
  import System.Environment

  import Control.Monad.Except
  import Control.Monad.State
  import Data.Functor.Identity


  readFile' :: FilePath -> ExceptT String IO String
  readFile' path = do
    exists <- liftIO $ doesFileExist path
    if exists
      then do
        content <- liftIO $ readFile path
        return content
      else 
        throwError ("File " ++ path ++ " does not exists")


  parseFile :: FilePath -> ExceptT String IO Program
  parseFile path = do
    content <- readFile' path
    case (parse content) of 
      Left e -> throwError (show e)
      Right x -> return x

  -- For Brainfuck loops :
  -- Execute loop content while current cell value is greater than 0

  processArgs :: [String] -> [(String, String)]
  processArgs [] = []
  processArgs (x:y:xs) = if (isPrefixOf "-" x) then (x, y) : processArgs xs else processArgs xs
  processArgs (x:xs) = if (not . isPrefixOf "-" $ x) then [] else processArgs xs
  
  getArg :: String -> [(String, String)] -> Maybe (String, String)
  getArg x = find ((==x) . fst)
  
  getNewStack :: Maybe (String, String) -> Stack
  getNewStack Nothing = initStack 255
  getNewStack (Just (_, s)) = do
    let size = read s :: Int
    initStack (size - 1)
  
  main :: IO ()
  main = do
    args <- getArgs
    let processed = processArgs (drop 1 args)
    parsed <- runExceptT (parseFile (args !! 0))
    case parsed of
      Left e -> fail e
      Right x -> do
        _ <- runStateT (eval x) (getNewStack (getArg "-s" processed), 0)
        return ()
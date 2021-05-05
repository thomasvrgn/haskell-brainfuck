module Main where
  import Core.Parser
  import Text.Parsec.Error
  import Control.Monad.Except
  import System.Directory
  import Types.Element
  import Core.Build

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

  main :: IO ()
  main = do
    res <- runExceptT (parseFile "tests/Hello.bf")
    case res of
      Left e -> fail e
      Right x -> putStrLn (buildFromTree x)
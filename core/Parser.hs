module Core.Parser where
  import Text.Parsec
  import Text.Parsec.String
  import Types.Element

  -- Skipping spaces but parsing elements which may be separated by space
  parseLine :: Parser Program
  parseLine = skipMany space >> sepEndBy parseElement spaces

  -- Matching Brainfuck operators
  matchChar :: Parser Char
  matchChar = 
    char '+' <|>
    char '-' <|>
    char '>' <|>
    char '<' <|>
    char '.' <|>
    char ',' <|>
    noneOf "]"

  -- Composing and creating a Element data builder
  parseOperator :: Parser Element
  parseOperator = set <$> matchChar
    where set '+' = Increment
          set '-' = Decrement
          set '>' = Next
          set '<' = Previous
          set '.' = Output
          set ',' = Input
          set _ = Comment

  parseElement :: Parser Element
  parseElement =  parseLoop <|> parseOperator 
  
  parseLoop :: Parser Element
  parseLoop = Loop <$> between (char '[') (char ']') parseLine

  -- Running parser which can end if eof throws an error (e.g: mismatched brackets)
  parse :: String -> Either ParseError Program
  parse s = removeComments <$> parsed
    where parsed = runP (parseLine <* eof) () "" s

  -- Recursively removing comments from abstract tree
  removeComments :: Program -> Program
  removeComments [] = []
  removeComments (Comment:xs) = removeComments xs
  removeComments (x:xs) = x : removeComments xs
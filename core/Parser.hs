module Core.Parser where
  import Text.Parsec
  import Text.Parsec.String
  import Types.Element

  parseLine :: Parser Program
  parseLine = skipMany space >> sepEndBy parseElement spaces

  matchChar :: Parser Char
  matchChar = 
    char '+' <|>
    char '-' <|>
    char '>' <|>
    char '<' <|>
    char '.' <|>
    char ',' <|>
    noneOf "]"

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

  parse :: String -> Either ParseError Program
  parse = runP (parseLine <* eof) () ""

  removeComments :: Program -> Program
  removeComments [] = []
  removeComments (Comment:xs) = removeComments xs
  removeComments (x:xs) = x : removeComments xs
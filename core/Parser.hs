module Core.Parser where
  import Text.Parsec
  import Text.Parsec.ByteString.Lazy
  import Types.Element
  
  type Program = [Element]

  matchChar :: Parser Char
  matchChar = 
    char '+' <|>
    char '-' <|>
    char '>' <|>
    char '<' <|>
    char '.' <|>
    char ','

  setOperator :: Parser Element
  setOperator = set <$> matchChar
    where set '+' = Increment
          set '-' = Decrement
          set '>' = Next
          set '<' = Previous
          set '.' = Output
          set ',' = Input
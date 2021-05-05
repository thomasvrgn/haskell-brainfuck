module Core.Build where
  import Types.Element
  import Control.Monad.Except

  buildFromTree :: Program -> String
  buildFromTree [] = []
  buildFromTree (x:xs) = (case x of
    Loop a -> ("[" ++ buildFromTree a ++ "]")
    Increment -> "+"
    Decrement -> "-"
    Next -> ">"
    Previous -> "<"
    Output -> "."
    Input -> ",") ++ buildFromTree xs
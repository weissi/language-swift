module Language.Swift.Parser.SourceLocation where

data TokenSpan =
    TokenSpan !Int -- address (number of characters preceding the token)
              !Int -- line number
              !Int -- column
              deriving (Eq, Show, Read)

tokenSpanEmpty :: TokenSpan
tokenSpanEmpty = TokenSpan (-1) (-1) (-1)

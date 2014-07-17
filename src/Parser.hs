{-| Module Parser contains  a parser which allows error sensible parsing of 
lists in haskell syntax.

Example:  Parsing a list of integers:
> parseIntList "[1,2,3,4]" ==> [1,2,3,4] 

 -}
module Parser (parseIntList) where
import Prelude hiding (readList)
import Text.ParserCombinators.Parsec hiding (Parser)
import Control.Applicative hiding ((<|>))

-- |parse function for integer lists
parseIntList :: String -> Either ParseError [Int]
parseIntList s = map read <$> runParser (list (some digit)) () [] s 

type Parser a = GenParser Char () a

braced ::  Parser t -> Parser t
braced = between (char '[') (char ']') 

-- |parses a list of elements. 
list :: Parser t  -- ^ the element parser
     -> Parser [t] 
list p = try (braced  (return []) )
     <|> braced (innerList p)

-- |parses a sequence of elements separated by comma 
-- (without surrounding brackets)
innerList :: Parser t  -- ^ the element parser
          -> Parser [t]
innerList pElement = 
      try ((:) <$> (pElement <* char ',') <*> innerList pElement)
  <|> (return <$> pElement)


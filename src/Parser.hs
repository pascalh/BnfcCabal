{-| Module Parser contains  a parser which allows error sensible parsing of 
lists in haskell syntax.

Example:  Parsing a list of integers:
> parseIntList "[1,2,3,4]" ==> [1,2,3,4] 

 -}
module Parser (parseIntList) where
import Prelude hiding (readList)
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

-- |parse function for integer lists
parseIntList :: String -> Either ParseError [Int]
parseIntList = 
  readList (read::String->Int) (list (some digit)) . filter (/=' ')

-- |parses a list in haskells list syntax
readList
  :: (a -> b) -- ^ how to transform one token of input list to destination type 
  -> GenParser c () [a] -- ^ parser
  -> [c]                -- ^ input token list
  -> Either ParseError [b]
readList r p s = 
  case runParser p () [] s of
    Right xs -> Right $ map r xs
    Left e   -> Left e 

-- |parsers accepting braces
lBrace, rBrace :: GenParser Char st Char 
lBrace = char '['
rBrace = char ']'

-- |parses a list of elements. 
list :: GenParser Char st t  -- ^ the element parser
     -> GenParser Char st [t] 
list p = try ([] <$ lBrace <* rBrace) 
     <|> lBrace *> innerList p <* rBrace 

-- |parses a sequence of elements separated by comma 
-- (without surrounding brackets)
innerList :: GenParser Char st t  -- ^ the element parser
          -> GenParser Char st [t]
innerList pElement = 
      try ((:) <$> (pElement <* char ',') <*> innerList pElement)
  <|> (return <$> pElement)


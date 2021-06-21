module BrainFuck.Parser (
  BFOp (..), 
  BFProgram, 
  program, 
  operation, 
  loop, 
  parseBF
)
  where 

import Text.Parsec.String (Parser)
import Text.Parsec.Prim (skipMany, (<|>), runP )
import Text.Parsec.Combinator ( between, sepBy, eof)
import Text.Parsec.Char( space, spaces, char )
import Text.Parsec.Error(ParseError)

import Control.Applicative ((<*), (<$>), many )

import BrainFuck.Type(BFOp(..), BFProgram)


program :: Parser BFProgram
program = program' <* eof 

program' :: Parser BFProgram
program' = do 
  skipMany space
  (operation <|> loop)  `sepBy` spaces 


operation :: Parser BFOp
operation = charToOperation <$> reservedChar 
  where 
    reservedChar = char '>' <|> char '<' <|> char '+' <|> char '-' <|> char '.' <|> char ','
    charToOperation c = case c of 
        '>' -> MoveRight
        '<' -> MoveLeft
        '+' -> Increment
        '-' -> Decrement 
        '.' -> Output
        ',' -> Replace


loop :: Parser BFOp
loop = Loop <$> between (char '[') (char ']') program

parseBF :: String -> Either ParseError BFProgram
parseBF = runP program () ""
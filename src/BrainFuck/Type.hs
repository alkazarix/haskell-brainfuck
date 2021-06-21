module BrainFuck.Type 
(
  BFOp(..), 
  BFProgram, 
  BFMachine(..),
  BFError (..), 
  BFEvalResult (..)
)
  where 

import Data.Int(Int8)
import Text.ParserCombinators.Parsec.Error(ParseError (..))

data BFOp = Increment
      | Decrement
      | MoveRight 
      | MoveLeft 
      | Replace 
      | Output  
      | Loop [BFOp]
    deriving (Show)

type BFProgram = [BFOp]


type MemoryCell = Int8
type MemoryOffset = Int

data BFMachine = BFM {
  _memory ::[MemoryCell],
  _pointer :: MemoryOffset
}


data BFError = BFParseError ParseError | BFExecutionError String
  deriving (Show)

type BFEvalResult = Either BFError BFMachine
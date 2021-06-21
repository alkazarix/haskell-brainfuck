module BrainFuck.Operation (
  right, 
  left, 
  increment, 
  decrement, 
  getByte,
  setByte 
)
  where

import BrainFuck.Type(BFMachine ( .. ), BFError(..))
import Data.Int(Int8)

right :: BFMachine -> Either BFError BFMachine 
right = moveWith (+1)

left :: BFMachine -> Either BFError BFMachine
left = moveWith (+ (-1))

increment :: BFMachine -> BFMachine
increment  = updateByteWith (+1)

decrement :: BFMachine -> BFMachine
decrement  = updateByteWith (+ (-1))

getByte :: BFMachine -> Int8
getByte (BFM mem pointer) = x
  where 
    (x:xs) = drop pointer mem

setByte :: Int8 -> BFMachine -> BFMachine 
setByte v (BFM mem pointer) = BFM mem' pointer
  where
    (x:xs) = drop pointer mem
    mem' = take pointer mem ++ [v] ++ xs  

moveWith :: (Int -> Int) -> BFMachine -> Either BFError BFMachine
moveWith f (BFM mem pointer) = case f pointer of
  n | n >= 0 -> Right $ BFM mem n
  _         ->  Left $ BFExecutionError ""

updateByteWith :: (Int8 -> Int8) -> BFMachine -> BFMachine
updateByteWith f (BFM mem pointer) =  BFM (mem' pointer mem f x xs) pointer
  where
  (x:xs) = drop pointer mem
  mem' pointer mem f x xs = take pointer mem  ++ [f x] ++ xs

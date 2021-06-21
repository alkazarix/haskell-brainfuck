module BrainFuck.Eval
where

import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Control.Monad(foldM, liftM)
import Control.Monad.IO.Class(liftIO)

import BrainFuck.Type (BFMachine (..), BFOp(..), BFProgram(..), BFError(..))
import BrainFuck.Parser (parseBF)

import BrainFuck.Operation

type BFEvalResult = ExceptT BFError IO BFMachine


createVM :: BFMachine
createVM = BFM initMemory 0
  where initMemory = repeat 0


evalString :: String -> BFEvalResult
evalString str = liftEither (mapErr $ parseBF str) >>= evalProgram
  where
    mapErr result = case result of
      Left err -> Left $ BFParseError err
      Right m  -> Right m


evalProgram :: BFProgram -> BFEvalResult
evalProgram = foldM (flip $ evalOP) createVM

evalOP ::  BFOp -> BFMachine -> BFEvalResult
evalOP Increment m = liftEither . Right . increment $ m 
evalOP Decrement m = liftEither . Right . decrement $ m
evalOP MoveLeft  m = liftEither . left  $ m
evalOP MoveRight m = liftEither . right $ m

evalOP Output m = do
    let byte = getByte m
    liftIO $ (putChar . toEnum . fromIntegral) byte
    return m

evalOP Replace m = do
    byte <- liftIO $ (fmap (fromIntegral . fromEnum)) getChar
    let m' = setByte byte m
    return m'

--}

{--  
evalOP Increment  = toEvalutionState $ (Right . Op.increment) 
evalOP Decrement  = toEvalutionState $ (Right . Op.decrement) 
evalOP MoveRight  = toEvalutionState $ (Right . Op.right )  
evalOP MoveLeft   = toEvalutionState $ (Right . Op.left)  
evalOP Output = do 
    result  <- get 
    return undefined 
       


evalOP Replace    = undefined


toEvalutionState :: (BFMachine -> BFEvalResult) -> BFEvalState
toEvalutionState f = StateT $ \result -> case result of
                        Left error    -> return $ ((), Left error)
                        Right machine -> return $ ((), f machin

--}



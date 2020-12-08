{-# LANGUAGE RecordWildCards #-}
module Lib.GameConsole where

import Data.Set ( Set, empty, insert )

data Instruction
    = Acc Int
    | Jmp Int
    | NOp Int
    deriving (Show, Eq, Ord)

fromString :: (String, String) -> Instruction
fromString ("nop", '+':xs) = NOp (read xs)
fromString ("nop", '-':xs) = NOp (negate $ read xs)
fromString ("acc", '+':xs) = Acc (read xs)
fromString ("acc", '-':xs) = Acc (negate $ read xs)
fromString ("jmp", '+':xs) = Jmp (read xs)
fromString ("jmp", '-':xs) = Jmp (negate $ read xs)
fromString t = error $ "Ye kya hai bc" ++ show t


data ProgramState = ProgramState
    { acc :: Int
    , pc :: Int
    , executed :: Set (Int, Instruction)
    } deriving (Show, Eq)


defaultProgramState :: ProgramState
defaultProgramState = ProgramState { acc = 0, pc = 0, executed = empty }

-- >>> runInstruction (NOp 0)  defaultProgramState
-- ProgramState {acc = 0, pc = 1, executed = fromList [(0,NOp 0)]}
runInstruction :: Instruction -> ProgramState -> ProgramState
runInstruction i@(Acc v) ps@ProgramState {..} = ps { acc = acc + v, pc = pc + 1, executed = insert (pc, i) executed }
runInstruction i@(NOp _) ps@ProgramState {..} = ps { pc = pc + 1, executed = insert (pc, i) executed }
runInstruction i@(Jmp v) ps@ProgramState {..} = ps { pc = pc + v, executed = insert (pc, i) executed }


alreadyExecuted :: (Int, Instruction) -> ProgramState -> Bool
alreadyExecuted instruction ProgramState {..} = instruction `elem` executed

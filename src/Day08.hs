{-# LANGUAGE RecordWildCards #-}
module Day08 (d8p1, d8p2) where

import Data.Vector (Vector, (!), fromList, (//))

import Lib.ListUtils ( tuplify )
import Lib.GameConsole
    ( ProgramState(..)
    , Instruction(NOp, Jmp)
    , fromString
    , defaultProgramState
    , alreadyExecuted
    , runInstruction
    )


-- >>> runProgram defaultProgramState (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6])
-- ProgramState {acc = 5, pc = 1, executed = fromList [(0,NOp 0),(1,Acc 1),(2,Jmp 4),(3,Acc 3),(4,Jmp (-3)),(6,Acc 1),(7,Jmp (-4))]}
runProgramUntilRepetation :: ProgramState -> Vector Instruction -> ProgramState
runProgramUntilRepetation ps@ProgramState {..} instructions
  | (pc, instructinToExecute) `alreadyExecuted` ps = ps
  | pc == length instructions = ps
  | otherwise = runProgramUntilRepetation newState instructions
    where
        instructinToExecute = instructions ! pc
        newState = runInstruction instructinToExecute ps

-- >>> parse "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"
-- [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6]
parse :: String -> [Instruction]
parse = fmap (fromString . tuplify . words) . lines

p1 :: [Instruction] -> Int
p1 = acc . runProgramUntilRepetation defaultProgramState . fromList

d8p1 :: IO ()
d8p1 = getContents >>= print . p1 .  parse


-- Part 2

-- >>> hasLoop defaultProgramState (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6])
-- True
-- >>> hasLoop defaultProgramState (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,NOp (-4),Acc 6])
-- False
hasLoop :: ProgramState -> Vector Instruction -> Bool
hasLoop ps@ProgramState {..} instructions
  | (pc, instructinToExecute) `alreadyExecuted` ps = True
  | pc == length instructions = False
  | otherwise = hasLoop newState instructions
    where
        instructinToExecute = instructions ! pc
        newState = runInstruction instructinToExecute ps

swapJmpNOp :: Instruction -> Instruction
swapJmpNOp (NOp v) = Jmp v
swapJmpNOp (Jmp v) = NOp v
swapJmpNOp x = x

-- >>> flipInstructionAt 0 (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6])
-- [Jmp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6]
-- >>> flipInstructionAt 1 (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6])
-- [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6]
-- >>> flipInstructionAt 2 (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6])
-- [NOp 0,Acc 1,NOp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6]
swapJmpNOpAt :: Int -> Vector Instruction -> Vector Instruction
swapJmpNOpAt idx instructions = instructions // [(idx, changedInstruction)]
    where
        instruction = instructions ! idx
        changedInstruction = swapJmpNOp instruction

-- >>> removeLoop (fromList [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6])
-- [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,NOp (-4),Acc 6]
removeLoop :: Vector Instruction -> Vector Instruction
removeLoop originalInstructions = go 0
    where
        go idx
            | hasLoop defaultProgramState newInstructions = go (idx + 1)
            | otherwise = newInstructions
            where newInstructions = swapJmpNOpAt idx originalInstructions

-- >>> p2 [NOp 0,Acc 1,Jmp 4,Acc 3,Jmp (-3),Acc (-99),Acc 1,Jmp (-4),Acc 6]
-- 8
p2 :: [Instruction] -> Int
p2 = acc . runProgramUntilRepetation defaultProgramState . removeLoop . fromList

d8p2 :: IO ()
d8p2 = getContents >>= print . p2 .  parse

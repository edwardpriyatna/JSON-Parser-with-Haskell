{-# OPTIONS_GHC -Wno-missing-export-lists #-}
--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances
import Parser

data ADT = Empty
  deriving (Eq, Show)

-- | Exercise A

parseExerciseA :: Parser ADT
parseExerciseA = pure Empty

prettyPrintExerciseA :: ADT -> String 
prettyPrintExerciseA _ = "You must write a pretty printer!"

-- | Exercise B

parseExerciseB :: Parser ADT
parseExerciseB = pure Empty

prettyPrintExerciseB :: ADT -> String 
prettyPrintExerciseB _ = "You must write a pretty printer!"

-- | Exercise C


-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive _ = False

parseExerciseC :: Parser ADT
parseExerciseC = pure Empty

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC _ = "You must write a pretty printer!"
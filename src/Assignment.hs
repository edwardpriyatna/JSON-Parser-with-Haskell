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
import Control.Applicative
import Data.Char
import Data.List

data ADT = Empty
          |JsNum Int
          |JsStr String
          |JsBool Bool
          |JsList [ADT]
          |JsNot ADT
          |JsAnd ADT ADT
          |JsOr ADT ADT
          |JsAdd ADT ADT
          |JsSub ADT ADT
          |JsMul ADT ADT
          |JsDiv ADT ADT
          |JsPow ADT ADT
          |JsEq ADT ADT
          |JsNotEquals ADT ADT
          |JsGt ADT ADT
          |JsLt ADT ADT
          |JsTernary ADT ADT ADT
          |JsConst ADT ADT
          |JsConsts [ADT]
          |JsBlock [ADT]
          |JsonIfElse ADT ADT [ADT]
          |JsonFunctionCall String [ADT]
          |JsonReturn ADT
          |JsonFunction String [String] ADT
          deriving (Eq, Show)

-- | Exercise A

-- | Parse a specific string (used for reserved words or operators)
parseString :: String -> Parser String
parseString = traverse charTok

-- | Parse a span of characters that satisfy a predicate `f`
parseSpan :: (Char -> Bool) -> Parser String
parseSpan f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Result rest token

-- | Parse a string body, stopping when encountering a double quote
stringBody :: Parser String
stringBody = parseSpan (/= '"')

-- | Parse zero or more occurrences of an element separated by a separator
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- | Helper function for parsing unary operators such as '!'
jsonNotOperator :: Char -> (ADT -> ADT) -> Parser ADT
jsonNotOperator operatorSymbol constructor = do
  charTok '('
  spaces
  charTok operatorSymbol
  expr <- parseExerciseA
  spaces
  charTok ')'
  return (constructor expr)

-- | Helper function for parsing binary operators such as '+' and '-' 
jsonCharOperator :: Char -> (ADT -> ADT -> ADT) -> Parser ADT
jsonCharOperator operatorSymbol constructor = do
  charTok '('
  spaces
  expr1 <- parseExerciseA
  spaces
  charTok operatorSymbol
  spaces
  expr2 <- parseExerciseA
  spaces
  charTok ')'
  return $ constructor expr1 expr2

-- | Helper function for parsing binary operators that are strings like '&&', '||', etc.
jsonStringOperator :: String -> (ADT -> ADT -> ADT) -> Parser ADT
jsonStringOperator operatorSymbol constructor = do
  charTok '('
  spaces
  expr1 <- parseExerciseA
  spaces
  parseString operatorSymbol
  spaces
  expr2 <- parseExerciseA
  spaces
  charTok ')'
  return $ constructor expr1 expr2

-- | Parsers for different JSON constructs

jsonNumber :: Parser ADT
jsonNumber = JsNum <$> int
    
jsonString :: Parser ADT
jsonString = 
  (JsStr <$> (charTok '"' *> (stringBody <|> pure "") <* charTok '"')) 
  <|> 
  (JsStr <$> parseSpan isVarName)

jsonBool :: Parser ADT
jsonBool = f <$> (parseString "true" <|> parseString "false")
  where
    f "true" = JsBool True
    f "false" = JsBool False

jsonList :: Parser ADT
jsonList = JsList <$> (charTok '[' *> elements <* charTok ']')
  where
    elements = sepBy (charTok ',') (spaces *> parseExerciseA <* spaces)

-- | Parsers for JSON operations
jsonNot :: Parser ADT
jsonNot = jsonNotOperator '!' JsNot

jsonAnd :: Parser ADT
jsonAnd = jsonStringOperator "&&" JsAnd

jsonOr :: Parser ADT
jsonOr = jsonStringOperator "||" JsOr

jsonAdd :: Parser ADT
jsonAdd = jsonCharOperator '+' JsAdd

jsonSubtract :: Parser ADT
jsonSubtract = jsonCharOperator '-' JsSub

jsonMultiply :: Parser ADT
jsonMultiply = jsonCharOperator '*' JsMul

jsonDivide :: Parser ADT
jsonDivide = jsonCharOperator '/' JsDiv

jsonPower :: Parser ADT
jsonPower = jsonStringOperator "**" JsPow

jsonEquals :: Parser ADT
jsonEquals = jsonStringOperator "===" JsEq

jsonNotEquals :: Parser ADT
jsonNotEquals = jsonStringOperator "!==" JsNotEquals

jsonGreaterThan :: Parser ADT
jsonGreaterThan = jsonCharOperator '>' JsGt

jsonLessThan :: Parser ADT
jsonLessThan = jsonCharOperator '<' JsLt

jsonTernary :: Parser ADT
jsonTernary = do
  charTok '('
  spaces
  condition <- parseExerciseA
  spaces
  tok (is '?')
  spaces
  trueExpr <- parseExerciseA
  spaces
  tok (is ':')
  spaces
  falseExpr <- parseExerciseA
  spaces
  charTok ')'
  return $ JsTernary condition trueExpr falseExpr

-- | Main parser for Exercise A that tries all the different parsers
parseExerciseA :: Parser ADT
parseExerciseA = 
    jsonNumber 
    <|> jsonBool 
    <|> jsonList 
    <|> jsonNot 
    <|> jsonAnd 
    <|> jsonOr 
    <|> jsonAdd 
    <|> jsonSubtract 
    <|> jsonMultiply 
    <|> jsonDivide 
    <|> jsonPower 
    <|> jsonEquals 
    <|> jsonNotEquals 
    <|> jsonGreaterThan 
    <|> jsonLessThan 
    <|> jsonTernary 
    <|> jsonString

-- | Helper class to determine if a parsed structure should be printed in multiple lines for exercise A
class MultiLineA a where
    shouldUseMultiline :: a -> Bool
instance MultiLineA ADT where
    shouldUseMultiline (JsTernary condition trueExpr falseExpr) =
        let conditionStr = prettyPrintExerciseA condition
            trueExprStr = prettyPrintExerciseA trueExpr
            falseExprStr = prettyPrintExerciseA falseExpr
            combinedStr = conditionStr ++ trueExprStr ++ falseExprStr
            strLength = length combinedStr
        in strLength > 42
    shouldUseMultiline _ = False

-- | Pretty print functions for Exercise A structures
-- | Convert the parsed ADT structures into readable format

-- | Convert a list of ADTs into a string representation of a JSON array
prettyPrintList :: [ADT] -> String
prettyPrintList elements = "[" ++ intercalate ", " (map prettyPrintExerciseA elements) ++ "]"

-- | Main pretty print function for Exercise A. This function converts an ADT structure into a readable string
prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA (JsNum n) = show n  -- Convert JSON numbers directly using Haskell's show.
prettyPrintExerciseA (JsStr s) = show s  -- Convert JSON strings with quotes.
prettyPrintExerciseA (JsBool True) = "true"
prettyPrintExerciseA (JsBool False) = "false"
prettyPrintExerciseA (JsList elements) = prettyPrintList elements  -- Convert JSON list using the helper function.
prettyPrintExerciseA (JsNot expr) = "(!" ++ prettyPrintExerciseA expr ++ ")"  -- Convert JSON logical NOT.
-- For binary operations, surround with parentheses and insert the corresponding operator
prettyPrintExerciseA (JsAnd expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " && " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsOr expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " || " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsAdd expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " + " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsSub expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " - " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsMul expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " * " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsDiv expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " / " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsPow expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " ** " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsEq expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " === " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsNotEquals expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " !== " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsGt expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " > " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsLt expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " < " ++ prettyPrintExerciseA expr2 ++ ")"
-- For ternary conditional, determine if it should be multi-line based on the combined length of its parts
prettyPrintExerciseA (JsTernary condition trueExpr falseExpr) =
  let conditionStr = prettyPrintExerciseA condition
      trueExprStr = prettyPrintExerciseA trueExpr
      falseExprStr = prettyPrintExerciseA falseExpr
      combinedStr = conditionStr ++ trueExprStr ++ falseExprStr
      strLength = length combinedStr
      isMultiline = shouldUseMultiline (JsTernary condition trueExpr falseExpr)
      newlineBeforeQuestion = if isMultiline then "\n" else ""
      newlineBeforeColon = if isMultiline then "\n" else ""
  in
    "(" ++ conditionStr ++ newlineBeforeQuestion ++ " ? " ++
    trueExprStr ++ newlineBeforeColon ++ " : " ++ falseExprStr ++ ")"

-- | Exercise B

-- | Determine if a character is a valid variable name character
isVarName :: Char -> Bool
isVarName c = isAlphaNum c || c == '_'

-- | Helper function to parse a single statement in a block
-- | This can be a constant declaration, an if-else block, or a code block
statement :: Parser ADT
statement = jsonConst <|> jsonIfElse <|> jsonBlock

-- | Helper function to parse a simple condition, which is any valid expression in this context
simpleCondition :: Parser ADT
simpleCondition = do
    cond <- parseExerciseA
    return cond

-- | Helper function to parse the "else" block of an if-else statement
falseBlockOption :: Parser ADT
falseBlockOption = do
    _ <- parseString "else"
    spaces
    block <- jsonBlock
    return block

-- | Parser for constant declarations in the format: const variable_name = value;
jsonConst :: Parser ADT
jsonConst = do
  _ <- parseString "const"
  spaces
  name <- parseSpan isVarName
  spaces
  charTok '='
  spaces
  value <- parseExerciseA
  spaces
  charTok ';'
  return $ JsConst (JsStr name) value

-- | Parser for multiple constant declarations
jsonConsts :: Parser ADT
jsonConsts = do
  consts <- sepBy spaces jsonConst
  return $ JsConsts consts

-- | Parser for a block of code enclosed in curly braces
jsonBlock :: Parser ADT
jsonBlock = do
    _ <- charTok '{'
    statements <- sepBy spaces statement
    _ <- charTok '}'
    return $ JsBlock statements
  
-- | Parser for if-else statements
jsonIfElse :: Parser ADT
jsonIfElse = do
    _ <- parseString "if"
    spaces
    _ <- charTok '('
    condition <- simpleCondition
    _ <- charTok ')'
    spaces
    trueBlock <- jsonBlock
    spaces
    falseBlockMaybe <- optional falseBlockOption
    let falseBlocks = case falseBlockMaybe of
                        Just blk -> [blk]
                        Nothing  -> []
    return $ JsonIfElse condition trueBlock falseBlocks

-- | Main parser for Exercise B
parseExerciseB :: Parser ADT
parseExerciseB = jsonBlock <|> jsonIfElse <|> jsonConsts 

-- | Helper class to determine if a parsed structure should be printed in multiple lines for Exercise B
class MultiLineB b where
    shouldUseMultilineB :: b -> Bool
instance MultiLineB ADT where
    shouldUseMultilineB adt = length (simplePrintB adt) > 42
      where
        simplePrintB (JsTernary condition trueExpr falseExpr) =
          prettyPrintExerciseB condition ++
          prettyPrintExerciseB trueExpr ++
          prettyPrintExerciseB falseExpr
        simplePrintB other = prettyPrintExerciseB other

-- | Pretty print functions for Exercise B structures
-- | Convert the parsed ADT structures into readable format

-- | Pretty print a block of code, deciding whether to use multiple lines based on the content
prettyPrintJsBlock :: [ADT] -> String
prettyPrintJsBlock adts
    -- If any statement in the block should be multiline or if there are multiple statements, use the multiline format.
    | isMultilineBlock = "{\n" ++ unlines (map (\adt -> "  " ++ prettyPrintExerciseB adt ++ ";") adts) ++ "}"
    -- Otherwise, use the inline format.
    | otherwise        = "{ " ++ intercalate "; " (map prettyPrintExerciseB adts) ++ (if not (null adts) then "; " else "") ++ "}"
  where
    -- Determine if the block should be printed in multiple lines.
    isMultilineBlock = any shouldUseMultilineB adts || length adts > 1

-- | Pretty print the contents of a block for multiline format
prettyPrintContents :: [ADT] -> String
prettyPrintContents adts = "{\n" ++ unlines (map (\adt -> "  " ++ prettyPrintExerciseB adt) adts) ++ "\n}"

-- | Pretty print the contents of a block for inline format
prettyPrintInlineContents :: [ADT] -> String
prettyPrintInlineContents adts = "{ " ++ intercalate "; " (map prettyPrintExerciseB adts) ++ " }"

-- | Helper function to determine how to pretty print a block or a single statement
prettyPrintBlockOrContent :: ADT -> String
prettyPrintBlockOrContent adt@(JsBlock _) = prettyPrintExerciseB adt
prettyPrintBlockOrContent adt
    | shouldUseMultilineB adt = prettyPrintContents [adt]
    | otherwise               = prettyPrintInlineContents [adt]

-- | Pretty print if-else statements
prettyPrintIfElse :: ADT -> String
prettyPrintIfElse (JsonIfElse condition trueBlock []) =
    "if ( " ++ prettyPrintExerciseA condition ++ " ) " ++ prettyPrintBlockOrContent trueBlock
prettyPrintIfElse (JsonIfElse condition trueBlock falseBlocks) =
    "if ( " ++ prettyPrintExerciseA condition ++ " ) "
    ++ prettyPrintBlockOrContent trueBlock
    -- Add a newline for multiline blocks, or a space for inline format.
    ++ (if shouldUseMultilineB trueBlock || any shouldUseMultilineB falseBlocks then "\n" else " ")
    ++ "else " ++ prettyPrintBlockOrContent (head falseBlocks)

-- | Main pretty print function for Exercise B
prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (JsConst (JsStr name) value) =
  "const " ++ name ++ " = " ++ prettyPrintExerciseA value
prettyPrintExerciseB (JsConsts consts) =
  intercalate "\n" (map prettyPrintExerciseB consts)
prettyPrintExerciseB (JsBlock adts) = prettyPrintJsBlock adts
prettyPrintExerciseB (JsonIfElse cond trueBlock falseBlocks) =
  prettyPrintIfElse (JsonIfElse cond trueBlock falseBlocks)
-- Fallback to Exercise A's pretty print function for other structures.
prettyPrintExerciseB other = prettyPrintExerciseA other

-- | Exercise C

-- | Parser for function names 
functionName :: Parser String
functionName = parseSpan isVarName

-- | Parser for function call arguments
functionArguments :: Parser [ADT]
functionArguments = sepBy (charTok ',') (spaces *> parseExerciseA <* spaces)

jsonReturn :: Parser ADT
jsonReturn = do
    _ <- string "return"
    _ <- spaces
    expr <- parseExerciseA
    _ <- charTok ';'
    return $ JsonReturn expr
  
jsonFunctionCall :: Parser ADT
jsonFunctionCall = do
    _ <- spaces
    fname <- functionName
    _ <- spaces
    _ <- is '('
    _ <- spaces
    args <- functionArguments
    _ <- spaces
    _ <- is ')'
    _ <- spaces
    _ <- is ';'
    _ <- spaces
    return $ JsonFunctionCall fname args

jsonFunction :: Parser ADT
jsonFunction = do
    _ <- stringTok "function"
    fname <- functionName
    _ <- charTok '('
    params <- sepBy (charTok ',') functionName
    _ <- charTok ')'
    body <- jsonBlock
    return $ JsonFunction fname params body

parseExerciseC :: Parser ADT
parseExerciseC = jsonFunction <|> jsonFunctionCall

extractReturns :: ADT -> [ADT]
extractReturns (JsonReturn expr) = [expr]
extractReturns (JsBlock stmts) = concatMap extractReturns stmts
extractReturns _ = []

hasFunctionCalls :: ADT -> Bool
hasFunctionCalls (JsonFunctionCall _ _) = True
hasFunctionCalls (JsBlock stmts) = any hasFunctionCalls stmts
hasFunctionCalls (JsonFunction _ _ body) = hasFunctionCalls body
hasFunctionCalls _ = False

isRecursiveCall :: String -> ADT -> Bool
isRecursiveCall fname (JsonFunctionCall name args) = name == fname
isRecursiveCall _ _ = False

-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive contents =
    case parse parseExerciseC contents of
        Result _ adt -> isTailRecursiveADT adt
        _            -> False

isTailRecursiveADT :: ADT -> Bool
isTailRecursiveADT (JsonFunction fname params body) = 
    let returns = extractReturns body 
        nonRecursiveReturns = filter (not . isRecursiveCall fname) returns
        recursiveReturns = filter (isRecursiveCall fname) returns
    in null nonRecursiveReturns && length recursiveReturns == 1
isTailRecursiveADT _ = False

class MultiLineC c where
    shouldUseMultilineC :: c -> Bool
instance MultiLineC ADT where
    shouldUseMultilineC adt = length (simplePrintC adt) > 42
      where
        simplePrintC (JsonFunction _ _ body) =
          prettyPrintExerciseC body
        simplePrintC other = prettyPrintExerciseC other

prettyPrintBlock :: ADT -> String
prettyPrintBlock (JsBlock stmts) = "{ " ++ intercalate " " (map prettyPrintExerciseC stmts) ++ " }"
prettyPrintBlock other = prettyPrintExerciseC other

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC (JsonFunctionCall fname args) =
    fname ++ "(" ++ intercalate ", " (map prettyPrintExerciseA args) ++ ");"
prettyPrintExerciseC (JsonFunction name args body) =
  let header = "function " ++ name ++ "(" ++ intercalate ", " args ++ ") "
      isMultiline = shouldUseMultilineC body
      bodyPrint
        | isMultiline = "{\n" ++ (unlines . map ("  " ++) . lines $ prettyPrintBlock body) ++ "}"
        | otherwise   = prettyPrintBlock body
  in header ++ bodyPrint
prettyPrintExerciseC other = prettyPrintExerciseB other
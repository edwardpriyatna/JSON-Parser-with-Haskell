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

data ADT = Empty
          |JsonNumber Int
          |JsonString String
          |JsonBool Bool
          |JsonList [ADT]
          |JsonNot ADT
          |JsonAnd ADT ADT
          |JsonOr ADT ADT
          |JsonAdd ADT ADT
          |JsonSubtract ADT ADT
          |JsonMultiply ADT ADT
          |JsonDivide ADT ADT
          |JsonPower ADT ADT
          |JsonEquals ADT ADT
          |JsonNotEquals ADT ADT
          |JsonGreaterThan ADT ADT
          |JsonLessThan ADT ADT
          |JsonTernary ADT ADT ADT
          |JsonConst ADT ADT
          |JsonConsts [ADT]
          |JsonBlock [ADT]
          |JsonIfElse ADT ADT [ADT]
          deriving (Eq, Show)

-- | Exercise A

parseChar :: Char -> Parser Char
parseChar x = char >>= \y ->
  if x == y
    then return y
    else Parser $ \input -> Error (UnexpectedChar y)

parseString :: String -> Parser String
parseString = traverse parseChar

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Result rest token

jsonNumber :: Parser ADT
jsonNumber = JsonNumber <$> int
    
stringBody :: Parser String
stringBody = parseSpan (/= '"')

jsonString :: Parser ADT
jsonString = JsonString <$> (parseString "" *> stringBody <* parseString "")

jsonBool :: Parser ADT
jsonBool = f <$> (parseString "true" <|> parseString "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonList :: Parser ADT
jsonList = JsonList <$> (charTok '[' *> elements <* charTok ']')
  where
    elements = sepBy (charTok ',') (spaces *> parseExerciseA <* spaces)

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

prettyPrintList :: [ADT] -> String
prettyPrintList elements = "[" ++ intercalate ", " (map prettyPrintExerciseA elements) ++ "]"

jsonNotOperator :: Char -> (ADT -> ADT) -> Parser ADT
jsonNotOperator operatorSymbol constructor = do
  charTok '('
  spaces
  charTok operatorSymbol
  expr <- parseExerciseA
  spaces
  charTok ')'
  return (constructor expr)

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

jsonStringOperator :: String -> (ADT -> ADT -> ADT) -> Parser ADT
jsonStringOperator operatorSymbol constructor = do
  charTok '('
  spaces
  expr1 <- parseExerciseA
  spaces
  parseString operatorSymbol  -- Parse the operator as a string
  spaces
  expr2 <- parseExerciseA
  spaces
  charTok ')'
  return $ constructor expr1 expr2

jsonNot :: Parser ADT
jsonNot = jsonNotOperator '!' JsonNot

jsonAnd :: Parser ADT
jsonAnd = jsonStringOperator "&&" JsonAnd

jsonOr :: Parser ADT
jsonOr = jsonStringOperator "||" JsonOr

jsonAdd :: Parser ADT
jsonAdd = jsonCharOperator '+' JsonAdd

jsonSubtract :: Parser ADT
jsonSubtract = jsonCharOperator '-' JsonSubtract

jsonMultiply :: Parser ADT
jsonMultiply = jsonCharOperator '*' JsonMultiply

jsonDivide :: Parser ADT
jsonDivide = jsonCharOperator '/' JsonDivide

jsonPower :: Parser ADT
jsonPower = jsonStringOperator "**" JsonPower

jsonEquals :: Parser ADT
jsonEquals = jsonStringOperator "===" JsonEquals

jsonNotEquals :: Parser ADT
jsonNotEquals = jsonStringOperator "!==" JsonNotEquals

jsonGreaterThan :: Parser ADT
jsonGreaterThan = jsonCharOperator '>' JsonGreaterThan

jsonLessThan :: Parser ADT
jsonLessThan = jsonCharOperator '<' JsonLessThan

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
  return $ JsonTernary condition trueExpr falseExpr

parseExerciseA :: Parser ADT
parseExerciseA = jsonNumber <|> jsonBool <|> jsonList <|> jsonNot <|> jsonAnd <|> jsonOr <|> jsonAdd <|> jsonSubtract <|> jsonMultiply <|> jsonDivide <|> jsonPower <|> jsonEquals <|> jsonNotEquals <|> jsonGreaterThan <|> jsonLessThan <|> jsonTernary <|> jsonString 

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA (JsonNumber n) = show n
prettyPrintExerciseA (JsonString s) = show s
prettyPrintExerciseA (JsonBool True) = "true"
prettyPrintExerciseA (JsonBool False) = "false"
prettyPrintExerciseA (JsonList elements) = prettyPrintList elements
prettyPrintExerciseA (JsonNot expr) = "(!" ++ prettyPrintExerciseA expr ++ ")"
prettyPrintExerciseA (JsonAnd expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " && " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonOr expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " || " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonAdd expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " + " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonSubtract expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " - " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonMultiply expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " * " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonDivide expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " / " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonPower expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " ** " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonEquals expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " === " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonNotEquals expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " !== " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonGreaterThan expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " > " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonLessThan expr1 expr2) = "(" ++ prettyPrintExerciseA expr1 ++ " < " ++ prettyPrintExerciseA expr2 ++ ")"
prettyPrintExerciseA (JsonTernary condition trueExpr falseExpr) =
  let conditionStr = prettyPrintExerciseA condition
      trueExprStr = prettyPrintExerciseA trueExpr
      falseExprStr = prettyPrintExerciseA falseExpr
      combinedStr = conditionStr ++ trueExprStr ++ falseExprStr
      strLength = length combinedStr
      isMultiline = strLength > 42
      newlineBeforeQuestion = if isMultiline then "\n" else ""
      newlineBeforeColon = if isMultiline then "\n" else ""
  in
    "(" ++ conditionStr ++ newlineBeforeQuestion ++ " ? " ++
    trueExprStr ++ newlineBeforeColon ++ " : " ++ falseExprStr ++ ")"

-- | Exercise B

isVarName :: Char -> Bool
isVarName c = isAlphaNum c || c == '_'

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
  return $ JsonConst (JsonString name) value

jsonConsts :: Parser ADT
jsonConsts = do
  consts <- sepBy spaces jsonConst
  return $ JsonConsts consts

statement :: Parser ADT
statement = jsonConst 

jsonBlock :: Parser ADT
jsonBlock = do
    _ <- charTok '{'
    statements <- sepBy spaces statement
    _ <- charTok '}'
    return $ JsonBlock statements

isMultiline :: ADT -> Bool
isMultiline adt = length (prettyPrintExerciseB adt) > 42 || hasMultipleChildren adt || anyChildIsMultiline adt
  where
    hasMultipleChildren (JsonBlock stmts) = length stmts > 1
    hasMultipleChildren _                 = False

    anyChildIsMultiline (JsonBlock stmts) = any isMultiline stmts
    anyChildIsMultiline _                 = False

prettyPrintJsonBlock :: [ADT] -> String
prettyPrintJsonBlock adts
    | isMultilineBlock = "{\n" ++ unlines (map (\adt -> "  " ++ prettyPrintExerciseB adt) adts) ++ "}"
    | otherwise        = "{ " ++ intercalate " " (map prettyPrintExerciseB adts) ++ " }"
  where
    isMultilineBlock = any isMultiline adts || length combined > 42 || length adts > 1
    combined = concatMap prettyPrintExerciseB adts

simpleCondition :: Parser ADT
simpleCondition = do
    cond <- parseExerciseA
    return cond

falseBlockOption :: Parser ADT
falseBlockOption = do
    _ <- parseString "else"
    spaces
    block <- jsonBlock
    return block
  
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

shouldUseMultiline :: ADT -> Bool
shouldUseMultiline adt = isMultiline adt

prettyPrintContents :: [ADT] -> String
prettyPrintContents adts = "{\n" ++ unlines (map (\adt -> "  " ++ prettyPrintExerciseB adt) adts) ++ "\n}"

prettyPrintInlineContents :: [ADT] -> String
prettyPrintInlineContents adts = "{ " ++ intercalate " " (map prettyPrintExerciseB adts) ++ " }"

prettyPrintBlockOrContent :: ADT -> String
prettyPrintBlockOrContent (JsonBlock adts)
    | shouldUseMultiline (JsonBlock adts) = prettyPrintContents adts
    | otherwise = prettyPrintInlineContents adts
prettyPrintBlockOrContent other = prettyPrintJsonBlock [other]

prettyPrintIfElse :: ADT -> String
prettyPrintIfElse (JsonIfElse condition trueBlock []) =
    let 
        condStr = prettyPrintExerciseA condition
        trueStr = prettyPrintBlockOrContent trueBlock
    in
        if shouldUseMultiline condition || shouldUseMultiline trueBlock then
            "if ( " ++ condStr ++ " )\n{\n  " ++ trueStr ++ "\n}"
        else
            "if ( " ++ condStr ++ " ) { " ++ trueStr ++ " }"

prettyPrintIfElse (JsonIfElse condition trueBlock falseBlocks) =
    let 
        condStr = prettyPrintExerciseA condition
        trueStr = prettyPrintBlockOrContent trueBlock
        falseStr = prettyPrintBlockOrContent (head falseBlocks)
    in
        if shouldUseMultiline condition || shouldUseMultiline trueBlock || any shouldUseMultiline falseBlocks then
            "if ( " ++ condStr ++ " )\n{\n  " ++ trueStr ++ "\n}\nelse\n{\n  " ++ falseStr ++ "\n}"
        else
            "if ( " ++ condStr ++ " ) { " ++ trueStr ++ " } else { " ++ falseStr ++ " }"

parseExerciseB :: Parser ADT
parseExerciseB = jsonIfElse <|> jsonBlock <|> jsonConsts 

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (JsonConst (JsonString name) value) =
  "const " ++ name ++ " = " ++ prettyPrintExerciseA value ++ ";"
prettyPrintExerciseB (JsonConsts consts) =
  intercalate "\n" (map prettyPrintExerciseB consts)
prettyPrintExerciseB (JsonBlock adts) = prettyPrintJsonBlock adts
prettyPrintExerciseB (JsonIfElse cond trueBlock falseBlocks) =
  prettyPrintIfElse (JsonIfElse cond trueBlock falseBlocks)
prettyPrintExerciseB other = prettyPrintExerciseA other

-- | Exercise C

-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive _ = False

parseExerciseC :: Parser ADT
parseExerciseC = pure Empty

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC _ = "You must write a pretty printer!"

main :: IO ()
main = do
    let expressionsA = [
            JsonNumber  123, -- 123
            JsonNumber (-123), -- -123
            JsonString "abc", -- "abc"
            JsonBool True, -- true
            JsonList [JsonNumber 1, JsonNumber 2 , JsonNumber 3], -- [1, 2, 3]
            JsonList [JsonBool True, JsonBool False, JsonNumber 1], -- [true, false, 1]
            JsonNot (JsonBool True), -- (!true)
            JsonAnd (JsonNot (JsonBool False)) (JsonBool True), -- ((!false) && true)
            JsonEquals (JsonMultiply (JsonAdd (JsonNumber 4) (JsonNumber 3)) (JsonNumber 2)) (JsonAdd (JsonNumber 4) (JsonMultiply (JsonNumber 3) (JsonNumber 2))), -- (((4 + 3) * 2) === (4 + (3 * 2)))
            JsonTernary (JsonNumber 1) (JsonNumber 2) (JsonNumber 3), -- (1 ? 2 : 3)
            JsonTernary (JsonBool True) (JsonNumber 1) (JsonNumber 2), -- (true ? 1 : 2)
            JsonTernary (JsonGreaterThan (JsonNumber 1) (JsonNumber 2)) 
                      (JsonAdd (JsonNumber 1) (JsonNumber 2)) 
                      (JsonAdd (JsonNumber 3) (JsonNumber 4)), -- ((1 > 2) ? (1 + 2) : (3 + 4))
            JsonTernary 
                (JsonEquals 
                    (JsonMultiply (JsonAdd (JsonNumber 4) (JsonNumber 3)) (JsonNumber 2)) 
                    (JsonAdd (JsonNumber 4) (JsonMultiply (JsonNumber 3) (JsonNumber 2)))
                ) 
                (JsonAnd (JsonNot (JsonBool False)) (JsonBool True))
                (JsonNumber 0)
            -- ((((4 + 3) * 2) === (4 + (3 * 2)))\n? ((!false) && true)\n: 0)
          ]
    mapM_ (\expr -> putStrLn $ prettyPrintExerciseA expr) expressionsA
    let expressionsB = [
            JsonConst (JsonString "aVariable") (JsonNumber 4), -- const aVariable = 4;
            JsonConst (JsonString "a2_3aBcD") (JsonNumber 1), -- const a2_3aBcD = 1;
            JsonBlock [], -- { }
            JsonBlock [JsonConst (JsonString "variable") (JsonNumber 1)], -- { const variable = 1; }
            JsonBlock [
                JsonConst (JsonString "a") (JsonNumber 1),
                JsonIfElse 
                    (JsonString "a") 
                    (JsonBlock [JsonConst (JsonString "b") (JsonAdd (JsonString "a") (JsonNumber 1))]) 
                    []
            ], -- {\n const a = 1;\n if (a) { const b = (a + 1); } \n}
            JsonBlock [
                JsonConst (JsonString "variable") (JsonNumber 1),
                JsonConst (JsonString "variable2") (JsonNumber 2)
            ], -- {\n const variable = 1;\n const variable2 = 2; \n}
            JsonIfElse (JsonBool True) (JsonBlock []) [], 
            -- if (true) { }
            JsonIfElse (JsonNumber 1) (JsonBlock [JsonConst (JsonString "a") (JsonNumber 1)]) [JsonBlock [JsonConst (JsonString "b") (JsonNumber 2)]], 
            -- if (1) {\n const a = 1; \n} else { \nconst b = 2;\n\n}
            JsonIfElse (JsonBool True) (JsonBlock []) [JsonBlock [JsonConst (JsonString "a") (JsonNumber 1)]], 
            -- if ( true ) { } else { const a = 1; }
            JsonIfElse (JsonNumber 1) (JsonBlock [JsonConst (JsonString "a") (JsonNumber 1), JsonConst (JsonString "b") (JsonNumber 2)]) [],
            -- if (1){const a = 1;const b = 2; }
            JsonIfElse (JsonAnd (JsonBool True) (JsonBool False)) (JsonBlock [JsonConst (JsonString "a") (JsonNumber 1)]) [JsonBlock [JsonConst (JsonString "b") (JsonNumber 2), JsonIfElse (JsonBool True) (JsonBlock [JsonConst (JsonString "c") (JsonAdd (JsonString "b") (JsonNumber 1))]) []]]
            -- if ( (true && false) ){ \nconst a = 1;\n\n} else {\n const b = 2;\n\n if ( true ) { const c = (b + 1); }\n}
          ]
    mapM_ (\expr -> putStrLn $ prettyPrintExerciseB expr) expressionsB
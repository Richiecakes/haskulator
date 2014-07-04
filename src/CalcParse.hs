module CalcParse where

import Data.Char

import CalcTypes
import CalcParseLib
 
parseExpr :: Parse Char Expr
parseExpr = litParse `alt` opExpParse

opExpParse :: Parse Char Expr
opExpParse 
  = (token '(' >*>
     parseExpr >*>
     spot isOp >*>
     parseExpr >*>
     token ')') 
     `build` makeExpr

makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

isOp :: Char -> Bool
isOp ch = elem ch "+-*/%"

charToOp :: Char -> Ops
charToOp ch 
  = case ch of
      '+' -> Add
      '-' -> Sub
      '*' -> Mul
      '/' -> Div
      '%' -> Mod
      _   -> error "Operator does not exist"


litParse :: Parse Char Expr
litParse 
  = ((optional (token '~')) >*>
     (neList (spot isDigit)))
     `build` (charListToExpr.join) 
     where
     join = uncurry (++)

charListToExpr :: [Char] -> Expr
charListToExpr = Lit . charListToInt 

charListToInt :: [Char] -> Integer
charListToInt ('~':rest) = - (charListToNat rest)
charListToInt other = charListToNat other

charListToNat :: [Char] -> Integer
charListToNat [] = 0
charListToNat (ch:rest) 
  = charToNat ch * 10^(length rest) + charListToNat rest

charToNat :: Char -> Integer
charToNat ch =
    toInteger $
              if nch < n0 + 10 
                 then nch - n0
                 else n0
              where
                nch = fromEnum ch 
                n0  = fromEnum '0'						

topLevel :: Parse a b -> b -> [a] -> b
topLevel p defaultVal inp
  = case results of
      [] -> defaultVal
      _  -> head results
    where
    results = [ found | (found,[]) <- p inp ]

parseCommand :: Parse Char Command
parseCommand 
  = (parseExpr `build` Eval)
    `alt`
    endOfInput Null

calcLine :: String -> Command
calcLine = topLevel parseCommand Null
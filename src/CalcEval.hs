module CalcEval where

import CalcTypes

eval :: Expr -> Integer
eval (Lit n) = n
eval (Op op e1 e2)
  = opValue op v1 v2
    where
    v1 = eval e1
    v2 = eval e2

opValue :: Ops -> Integer -> Integer -> Integer
opValue Add = (+)
opValue Sub = (-) 
opValue Mul = (*) 
opValue Div = div 
opValue Mod = mod

command :: Command -> Maybe Integer
command Null     = Nothing
command (Eval e) = Just (eval e)
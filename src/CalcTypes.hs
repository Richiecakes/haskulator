module CalcTypes where

data Expr = Lit Integer | Op Ops Expr Expr deriving (Eq,Show)

data Ops  = Add | Sub | Mul | Div | Mod deriving (Eq,Show)

data Command = Eval Expr | Null	deriving (Eq,Show)
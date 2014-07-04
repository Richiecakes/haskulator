module CalcParseLib where

import Data.Char

infixr 5 >*>

type Parse a b = [a] -> [(b,[a])]
 
none :: Parse a b
none inp = []

succeed :: b -> Parse a b 
succeed val inp = [(val,inp)]

token :: Eq a => a -> Parse a a
token t (x:xs) 
  | t==x 	= [(t,xs)]
  | otherwise 	= []
token t []    = []

spot :: (a -> Bool) -> Parse a a
spot p (x:xs) 
  | p x 	= [(x,xs)]
  | otherwise 	= []
spot p []    = []

bracket = token '('
dig     =  spot isDigit

endOfInput :: b -> Parse a b
endOfInput x [] = [(x,[])]
endOfInput x _  = []

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp
exam1 = (bracket `alt` dig) "234" 

(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp 
  = [((y,z),rem2) | (y,rem1) <- p1 inp , (z,rem2)  <- p2 rem1 ]

build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [ (f x,rem) | (x,rem) <- p inp ]

list :: Parse a b -> Parse a [b]
list p = (succeed []) 
         `alt`
         ((p >*> list p) `build` convert)
         where
         convert = uncurry (:)

neList   :: Parse a b -> Parse a [b]
neList p = (p  `build` (:[]))
           `alt`
           ((p >*> list p) `build` (uncurry (:)))

optional :: Parse a b -> Parse a [b]
optional p = (succeed []) 
             `alt`  
             (p  `build` (:[]))

nTimes :: Int -> Parse a b -> Parse a [b]
nTimes 0 p     = succeed []
nTimes n p     = (p >*> nTimes (n-1) p) `build` (uncurry (:))
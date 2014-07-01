module CalcStore 
   ( Store, 
     initial,     -- Store
     value,       -- Store -> Var -> Integer
     update       -- Store -> Var -> Integer -> Store
    ) where

import CalcTypes					

data Store = Sto [ (Integer,Var) ] 

instance Eq Store where 
  (Sto sto1) == (Sto sto2) = (sto1 == sto2)					

instance Show Store where
  showsPrec n (Sto sto) = showsPrec n sto					

initial :: Store 
initial = Sto []

value  :: Store -> Var -> Integer
value (Sto []) v         = 0
value (Sto ((n,w):sto)) v 
  | v==w            = n
  | otherwise       = value (Sto sto) v

update  :: Store -> Var -> Integer -> Store
update (Sto sto) v n = Sto ((n,v):sto)
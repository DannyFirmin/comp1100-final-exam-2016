module Store
 (Store,
 initial, -- Store
 value,   -- Store -> Var -> Integer
 update   -- Store -> Var -> Integer -> Store
 ) where

data Store = Store [(Integer,Var)]

initial :: Store
initial = Store []

value :: Store -> Var -> Integer
value (Store []) v = 0
value (Store ((n,w):sto)) v
 |v==w =n
 |otherwise = value (Store sto) v

update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store ((n,v):sto)
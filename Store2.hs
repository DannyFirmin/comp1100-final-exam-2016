module Store2
 (Store,
 initial, -- Store
 value,   -- Store -> Var -> Integer
 update   -- Store -> Var -> Integer -> Store
 ) where

initial :: Store
initial = Store (\v -> 0)

value :: Store -> Var -> Integer
value (Store sto) v = sto v

update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store (\w -> if v==w then n else sto w)
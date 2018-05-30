module Questions where
--Q1
mystery :: (Eq a) => a -> a ->a -> Bool
mystery x y z = not ((x==y) && (y==z))

threeDifferent :: (Eq a) => a -> a -> a -> Bool
threeDifferent x y z = (x/=y) && (x/=z) && (y/=z)

fourEqual :: (Eq a) => a -> a -> a -> a -> Bool
fourEqual a b c d =threeDifferent a b c && threeDifferent b c d

--Q2
data Response = Link | Frame | Service

--Q3
data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)
norm :: Tree a -> Tree a
norm t = case t of
 Node Empty Empty -> Empty
 Node l Empty -> norm (norm l) Empty
 Node Empty r -> norm Empty (norm r)

--  data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)
--
--  norm::Tree a -> Tree a
--  norm Empty = Empty
--  norm (Leaf x) = Leaf x
--  norm (Node l r) = case (l,r) of
--      (Empty,Empty) ->Empty
--      (_,Empty) -> Empty
--      (Empty,_) ->Empty
--      (Leaf x,Leaf y) -> Node (Leaf x) (Leaf y)
--      (Leaf x, Node k m) -> Node (Leaf x) (norm(Node k m))

--Q4
type Point = (Float, Float)
type Constr = Point -> Bool

rect :: Point -> Point -> Constr
rect min max p
  | fst p >= fst min && fst p <= fst max = True
  | snd p >= snd min && snd p <= snd max = True
  |otherwise = False
-- Point -> Bool -> Point -> Bool -> Point -> Bool
intersection :: Constr -> Constr -> Constr
intersection c1 c2 = c1 `intersection` c2

union :: Constr -> Constr -> Constr
union c1 c2 = c1 `union` c2

inverse :: Constr -> Constr
inverse c1 c2 = c1 `inverse` c2

--Q5
data Tree2 a = NilT | Node2 a (Tree2 a) (Tree2 a)

elemT :: (Eq a) => Tree2 a -> a -> Bool
elemT NilT a = False
elemT (Node2 c l r) a
  |a==c = True
  |otherwise = elemT l a || elemT r a

elemBST :: (Ord a) => Tree2 a -> a -> Bool
elemBST NilT a = False
elemBST (Node2 c l r) a
  |a==c = True
  |a>=c = elemT r a
  |a<=c = elemT l a
  |otherwise = error"impossible BST!"
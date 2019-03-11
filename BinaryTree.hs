module BinaryTree(BinaryTree(Empty,Node), root, left, right, isEmpty) where

data BinaryTree t = Empty | Node t 
                            (BinaryTree t)
                            (BinaryTree t) 
                            deriving (Show, Read)
                            
root:: BinaryTree t -> t
root (Node x _ _) = x

left :: BinaryTree t -> BinaryTree t
left (Node _ l _) = l

right :: BinaryTree t -> BinaryTree t
right (Node _ _ r) = r

isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty _ = False
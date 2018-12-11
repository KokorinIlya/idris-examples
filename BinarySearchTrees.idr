module BinarySearchTrees

import ListUtils

export
data BinarySearchTree : Type -> Type where
  Empty : Ord t => BinarySearchTree t
  Node : Ord t => (left : BinarySearchTree t) -> (elem : t)
    -> (right : BinarySearchTree t) -> BinarySearchTree t

export
insert : BinarySearchTree t -> t -> BinarySearchTree t
insert Empty to_insert = Node Empty to_insert Empty
insert t@(Node left node_element right) to_insert =
  case (compare to_insert node_element) of
    LT => Node (insert left to_insert) node_element right
    EQ => t
    GT => Node left node_element (insert right to_insert)

export
list_to_tree : Ord t => List t -> BinarySearchTree t
list_to_tree [] = Empty
list_to_tree (x :: xs) =
  let tree = list_to_tree xs in
  insert tree x

export tree_to_list : BinarySearchTree t -> List t
tree_to_list Empty = []
tree_to_list (Node left elem right) =
  let left_list = tree_to_list left in
  let right_list = tree_to_list right in
  concat_lists left_list (elem :: right_list)

export find_in_tree : BinarySearchTree t -> t -> Bool
find_in_tree Empty to_find = False
find_in_tree (Node left elem right) to_find =
  case (compare to_find elem) of
    LT => find_in_tree left to_find
    EQ => True
    GT => find_in_tree right to_find

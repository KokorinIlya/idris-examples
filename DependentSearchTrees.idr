module DependentSearchTrees

import ListUtils

export
data DependentSearchTree : Bool -> Type -> Type where
  Empty : Ord t => DependentSearchTree True t
  Node : Ord t => (left : DependentSearchTree empty_left t) -> (elem : t)
    -> (right : DependentSearchTree empty_right t) -> DependentSearchTree False t

export
total insert : DependentSearchTree is_empty t -> t -> DependentSearchTree False t
insert Empty to_insert = Node Empty to_insert Empty
insert t@(Node left elem right) to_insert =
  case (compare to_insert elem) of
    LT =>
      let new_left = insert left to_insert in
      Node new_left elem right

    EQ => t

    GT =>
      let new_right = insert right to_insert in
      Node left elem new_right

export
total find : {is_empty : Bool} -> DependentSearchTree is_empty t -> t -> Bool
find {is_empty = True} Empty to_find = False
find {is_empty = False} (Node left elem right) to_find =
  case (compare to_find elem) of
    LT => find left to_find
    EQ => True
    GT => find right to_find

total is_list_empty : List t -> Bool
is_list_empty [] = True
is_list_empty (x :: xs) = False

total list_to_tree : Ord t => (list : List t) -> DependentSearchTree (is_list_empty list) t
list_to_tree [] = Empty
list_to_tree (x :: xs) =
  let t = list_to_tree xs in
  insert t x

total tree_to_list : {is_empty : Bool} -> DependentSearchTree is_empty t -> List t
tree_to_list {is_empty = True} Empty = []
tree_to_list {is_empty = False} (Node left elem right) =
  let left_list = tree_to_list left in
  let right_list = tree_to_list right in
  concat_lists left_list (elem :: right_list)

export
total tree_is_empty_after_delete : DependentSearchTree empty t -> t -> Bool
tree_is_empty_after_delete Empty to_delete = True
tree_is_empty_after_delete (Node left elem right) to_delete =
  case left of
    Node left_left left_elem left_right => False

    Empty => case right of
      Node right_left right_elem right_right => False

      Empty => case (compare to_delete elem) of
        LT => False
        EQ => True
        GT => False

-- find_min : Node t -> t
total find_min : {left_empty : Bool} ->
 DependentSearchTree left_empty t -> t -> DependentSearchTree right_empty t -> t
find_min {left_empty = True} Empty elem right = elem
find_min {left_empty = False} (Node left_left x left_right) elem right =
   find_min left_left x left_right


export
delete : (tree : DependentSearchTree is_empty t) -> (to_delete : t) ->
 DependentSearchTree (tree_is_empty_after_delete tree to_delete) t

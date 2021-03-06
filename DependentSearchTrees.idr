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

export
total tree_to_list : {is_empty : Bool} -> DependentSearchTree is_empty t -> List t
tree_to_list {is_empty = True} Empty = []
tree_to_list {is_empty = False} (Node left elem right) =
  let left_list = tree_to_list left in
  let right_list = tree_to_list right in
  concat_lists left_list (elem :: right_list)

export
total tree_is_empty_after_delete : DependentSearchTree empty t -> t -> Bool
tree_is_empty_after_delete Empty to_delete = True
tree_is_empty_after_delete (Node left elem right) to_delete with (compare to_delete elem)
    tree_is_empty_after_delete (Node left elem right) to_delete  | LT = False
    tree_is_empty_after_delete (Node left elem right) to_delete  | GT = False
    tree_is_empty_after_delete (Node Empty elem Empty) to_delete | EQ = True
    tree_is_empty_after_delete (Node left elem right) to_delete  | EQ = False

total find_max : DependentSearchTree False t -> t
find_max (Node left elem Empty) = elem
find_max (Node left elem right@(Node _ _ _)) = find_max right

export
delete : (tree : DependentSearchTree is_empty t) -> (to_delete : t) ->
 DependentSearchTree (tree_is_empty_after_delete tree to_delete) t
delete Empty to_delete = Empty
delete (Node left elem right) to_delete with (compare to_delete elem)
    delete (Node left elem right) to_delete  | LT =
      let new_left = delete left to_delete in
      Node new_left elem right

    delete (Node left elem right) to_delete  | GT =
      let new_right = delete right to_delete in
      Node left elem new_right

    delete (Node Empty elem Empty) to_delete | EQ =
      Empty

    delete (Node
      Empty
      elem
      right@(Node right_left right_elem right_right)
    ) to_delete | EQ =
      right

    delete (Node
      left@(Node left_left left_elem left_right)
      elem
      Empty
    ) to_delete | EQ =
      left

    delete (Node
        left@(Node left_left left_elem left_right)
        elem
        right@(Node right_left right_elem right_right)
    ) to_delee | EQ =
      let max_left = find_max left in
      let new_left = delete left max_left in
      Node new_left max_left right

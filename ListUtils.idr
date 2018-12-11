module ListUtils

export
concat_lists : List t -> List t -> List t
concat_lists [] ys = ys
concat_lists (x :: xs) ys = x :: (concat_lists xs ys)

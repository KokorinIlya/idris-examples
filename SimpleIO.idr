module SimpleIO

max_length : IO ()
max_length = do
  s1 <- getLine
  s2 <- getLine
  let l1 = length s1
  let l2 = length s2
  let max_l = if (l1 >= l2) then l1 else l2
  let max_l_str = show max_l
  putStrLn max_l_str

max_length_desugarised : IO ()
max_length_desugarised =
  getLine >>= ( \s1 =>
    let l1 = length s1 in
    getLine >>= ( \s2 =>
      let l2 = length s2 in
      let max_l = if (l1 >= l2) then l1 else l2 in
      let max_l_str = show max_l in
      putStrLn max_l_str
    )
  )

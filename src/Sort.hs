
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

qsort [] = []
qsort (x:xs) = (qsort lower) ++ [x] ++ (qsort upper)
    where
      lower = filter (<x) xs
      upper = filter (>=x) xs


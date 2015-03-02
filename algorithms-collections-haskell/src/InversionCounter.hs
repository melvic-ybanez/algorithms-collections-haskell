module InversionCounter where

count :: (Ord a) => [a] -> Int
count = snd . sortAndCount

sortAndCount :: (Ord a) => [a] -> ([a], Int)
sortAndCount [] = ([], 0)
sortAndCount [x] = ([x], 0)
sortAndCount xs = 
    let m = length xs `div` 2
        (lxs, lcount) = sortAndCount $ take m xs
        (rxs, rcount) = sortAndCount $ drop m xs
        (sxs, scount) = countAndSplitInversions lxs rxs
    in (sxs, lcount + rcount + scount)
        
countAndSplitInversions :: (Ord a) => [a] -> [a] -> ([a], Int)
countAndSplitInversions xs [] = (xs, 0)
countAndSplitInversions [] ys = (ys, 0)
countAndSplitInversions xs@(hx : tx) ys@(hy : ty)
    | hx <= hy = updateResult hx 0 . countAndSplitInversions tx $ ys
    | otherwise =  updateResult hy (length xs) . countAndSplitInversions xs $ ty  
    where updateResult h i (elems, n) = (h : elems, n + i)  
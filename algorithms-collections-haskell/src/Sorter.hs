module Sorter where

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = 
    let insert [] = [x]
        insert (x1 : xs1) 
            | x1 > x = x : x1 : xs1
            | otherwise = x1 : insert xs1
    in insert $ insertionSort xs
    
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let merge [] ys = ys
        merge xs1 [] = xs1
        merge xs1@(hx : txs1) ys@(hy : tys)
            | hx <= hy = hx : merge txs1 ys
            | otherwise = hy : merge xs1 tys
    in merge (mergeSort $ take m xs) (mergeSort $ drop m xs)
        where m = length xs `div` 2      
       

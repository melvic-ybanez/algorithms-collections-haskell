module Sorter where

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = insertionSort' (>) 
        
insertionSort' :: (a -> a -> Bool) -> [a] -> [a]
insertionSort' _ [] = []
insertionSort' gt (x : xs) = 
    let insert [] = [x]
        insert (x1 : xs1)
            | gt x1 x = x : x1 : xs1
            | otherwise = x1 : insert xs1
    in insert $ insertionSort' gt xs
    
mergeSort :: (Ord a) => [a] -> [a]
mergeSort = mergeSort' (<=)   

mergeSort' :: (a -> a -> Bool) -> [a] -> [a]
mergeSort' _ [] = []
mergeSort' _ [x] = [x]
mergeSort' lte xs = 
    let merge [] ys = ys
        merge xs1 [] = xs1
        merge xs1@(hx : txs1) ys@(hy : tys)
            | lte hx hy = hx : merge txs1 ys
            | otherwise = hy : merge xs1 tys
    in merge (mergeSort' lte $ take m xs) (mergeSort' lte $ drop m xs)
        where m = length xs `div` 2      
       

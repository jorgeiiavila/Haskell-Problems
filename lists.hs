rotateList :: [a] -> Int -> [a]
rotateList [] _ = []
rotateList list 0 = list
rotateList (x:xs) n = rotateList (xs ++ [x]) (n - 1)

removeNthElement :: [a] -> Int -> [a]
removeNthElement list n 
                    | n + 1 > length list = error "Out of bounds"
                    | otherwise = start ++ end 
                                    where 
                                        start = take n list
                                        end = drop (n + 1) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

printList :: (Show a) => [a] -> IO ()
printList [] = putStr ""
printList (x:xs) = do
    putStrLn (show x)
    printList xs

searchInList :: (Eq a) => [a] -> a -> Int
searchInList list n = searchInListHelper list n 0

searchInListHelper :: (Eq a) => [a] -> a -> Int -> Int
searchInListHelper [] _ _ = -1
searchInListHelper (x:xs) n index 
        | x == n = index
        | otherwise = searchInListHelper xs n (index + 1)  

customZip :: [a] -> [b] -> [(a, b)]
customZip [] _ = []
customZip _ [] = []
customZip (x:xs) (y:ys) = (x,y) : (customZip xs ys)


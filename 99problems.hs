-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast list = head (reverse list)

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (_:[]) = error "Just one element"
myButLast list = head (tail (reverse list))

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n | n < 1 = error "Index lower than one"
                   | otherwise = elementAt xs (n - 1)
-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = let zipped = zip list (reverse list)
                        in all (\(x,y) -> x == y) zipped

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ (flatten (List xs)) 
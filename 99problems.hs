import Data.List

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

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:ys@(y:_)) | x == y = compress ys
                  | otherwise = x : compress ys

-- Problem 9 - Option 1
-- pack :: (Eq a) => [a] -> [[a]]
-- pack xs = groupBy (\x y -> x == y) xs

-- Problem 9 - Option 2
pack list@(x:_) = let (first,rest) = span (==x) list
               in first : pack rest
pack [] = []

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

-- Problem 11
data Encoded a = Multiple Int a | Single a deriving Show
encodeModified :: (Show a, Eq a) => [a] -> [Encoded a]
encodeModified xs = map encodeModifiedHelper (pack xs)

encodeModifiedHelper :: (Eq a) => [a] -> Encoded a
encodeModifiedHelper xs | length xs == 1 = Single (head xs)
          | otherwise = Multiple (length xs) (head xs)

-- Problem 12 - My woeful solution
-- decodeModified :: [Encoded a] -> [a]
-- decodeModified [] = []
-- decodeModified ((Multiple l v):xs) = (replicate l v) ++ (decodeModified xs)
-- decodeModified ((Single v):xs) = [v] ++ (decodeModified xs)

-- Problem 12 - The elegant solution given by the wiki
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decodeHelper
                where decodeHelper (Single v) = [v]
                      decodeHelper (Multiple l v) = replicate l v
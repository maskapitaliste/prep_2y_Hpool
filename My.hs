import Data.List
mySucc n = n + 1
myIsNeg x = if x >= 0
   then False
   else True
myAbs x = if myIsNeg x == True
    then (-1) * (x)
    else x
myMin x y = if x > y
    then y
    else x
myMax x y = if x > y
    then x
    else y
myTuple a b = (a , b)
myTruple a b c= (a , b, c)
myFst (a, b) = a
mySnd (a , b) = b
mySwap (a , b) = (b , a)

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (head:tail) = head

myTail :: [a] -> [a]
myTail [] = error "empty list"
myTail (head:tail) = tail

myLength :: [a] -> Int
myLength [] = 0
myLength (head:tail) = 1 + myLength tail

myNth :: [a] -> Int -> a
myNth [] _ = error "Indehead too large"
myNth _ n | n < 0 = error "Negative indehead"
myNth (head:tail) 0 = head
myNth (head:tail) n
  | n > length tail = error "Indehead too large"
  | otherwise     = myNth tail (n - 1)

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 1  (head:tail) = tail
myTake 0 [a] = []
myTake i (head:tail)
  | i > 0 = head : myTake (i - 1) tail
  | otherwise = []

myDrop ::Int -> [ a ] -> [ a ]
myDrop 0 [a] = [a]
myDrop i [] = []
myDrop i (head:tail) = if i == 1
then tail
else myDrop (i-1) tail

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend ys [] = ys
myAppend (head:tail) ys = head : myAppend tail ys

myReverse :: [ a ] -> [ a ]
myReverse [] = []
myReverse (head:tail) = myAppend(myReverse tail) [head]

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit [x] = []
myInit (x:xs) = x : myInit xs

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (head:tail) = if myLength(head:tail) == 1 then head else myLast tail

myZip :: [a] -> [b] -> [(a, b)]
myZip [] [b] = []
myZip [a] [] = []
myZip (a:b) (c:d) = (a,c) : myZip b d

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y):xs) = (x:as, y:bs)
  where
    (as, bs) = myUnzip xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) = if p x then x : myFilter p xs else myFilter p xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition _ [] = ([], [])
myPartition p (x:xs) = if p x then (x:trues, falses) else (trues, x:falses)
  where
    (trues, falses) = myPartition p xs

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort p (x:xs) = myQuickSort p smaller ++ [x] ++ myQuickSort p larger
  where
    smaller = myFilter (< x) xs
    larger = myFilter (>= x) xs
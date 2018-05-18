-- PA5.hs
-- Nathaniel McDonald
-- 21 Mar 2018
--
-- CSCE A331 Spring 2018
-- Solutions to Assignment 5 Exercise B

module PA5 where

--counts the iterations of the collatz function for x
collatz :: Integer -> Integer
collatz a
          | (a == 1)          = 0
          | ((mod a 2) == 1)  = 1 + collatz (3*a + 1)
          | otherwise         = 1 + collatz (div a 2)

-- collatzCounts
-- counts the iterations of the collatz function for each integer greater than or 
-- equal to 1
collatzCounts :: [Integer]
collatzCounts = (map collatz [1,2..])


-- findList
findList :: Eq a => [a] -> [a] -> Maybe Int
findList _ [] = Nothing
findList xs (y:ys) 
	|(listLength (xs)) > (listLength (y:ys))  = Nothing
	|((xs) == (list_eq (xs) (y:ys)))          = Just 0
	|otherwise                                = (case findList (xs) ys of
							Just n -> Just (n + 1)
							Nothing -> Nothing)


--Takes 2 lists and returns the elements at corresponding indices containing
--equal values
list_eq _ [] = []
list_eq [] _ = []
list_eq (x:xs) (y:ys) = if x == y then x:(list_eq xs ys) else (list_eq xs ys)

--list length
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- operator ##
(##) :: Eq a => [a] -> [a] -> Int
xs ## ys = listLength (list_eq xs ys)


-- filterAB
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ [] _ = []
filterAB _ _ [] = []
filterAB (cond) (a:as) (b:bs) = if cond a then b:(filterAB cond as bs) else (filterAB cond as bs)

--Sequence used to filter even and odd elements of a list in sumEvenOdd
alt_seq = 0:1:(alt_seq)

 
-- sumEvenOdd
-- sums the even and the odd elements of a list
sumEvenOdd :: Num a => [a] -> (a, a)
sumEvenOdd a = (foldl (+) 0 (filterAB ((== 0)) alt_seq a), foldl (+) 0 (filterAB (== 1) alt_seq a))





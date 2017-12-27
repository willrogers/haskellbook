import Data.List


-- a version of a list
data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons a l) = a:(toList l)


-- a Tree type
data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)

treeVal :: Tree a -> a
treeVal (Tree x _ _) = x

treeHeight :: Tree a -> Int
treeHeight (Tree v Nothing Nothing) = 1
treeHeight (Tree v Nothing (Just (Tree a b c))) = 1 + treeHeight (Tree a b c)
treeHeight (Tree v (Just (Tree a b c)) Nothing) = 1 + treeHeight (Tree a b c)
treeHeight (Tree v (Just (Tree a b c)) (Just (Tree d e f))) = 1 + max (treeHeight (Tree a b c)) (treeHeight (Tree d e f))


-- a version of len
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs


mean :: (Real a, Fractional b) => [a] -> b
mean xs = (realToFrac (sum xs)) / genericLength xs

-- playing with palindromes
palin :: [a] -> [a]
palin [] = []
palin (x:[]) = [x, x]
palin (x:xs) = x:(palin xs) ++ [x]

isPalin :: Eq a => [a] -> Bool
isPalin [] = True
isPalin (x:[]) = True
isPalin (x:xs) =  (x == (last xs)) && (isPalin (init xs))


-- sort lists by length
sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength xs = sortBy lencmp xs
                where lencmp xs ys = compare (length xs) (length ys)


intersp :: a -> [[a]] -> [a]
intersp _ [] = []
intersp _ (y:[]) = y
intersp x (y:ys) = y ++ [x] ++ (intersp x ys)


main = do let n = Nil
          let l = Cons 1 Nil
          print $ l
          print $ fromList [1, 2, 3]
          print $ toList l
          let t = Tree "hello" Nothing (Just (Tree "asdf!" Nothing Nothing))
          print $ treeVal t
          print $ sortByLength [[1, 2, 3], [1], [1, 2]]
          print $ treeHeight t

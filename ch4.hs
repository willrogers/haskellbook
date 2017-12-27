import System.Environment (getArgs)
import Data.Char (digitToInt)

-- safe list operations
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = prep x (safeInit xs)
                where prep :: a -> Maybe [a] -> Maybe [a]
                      prep x Nothing = Nothing
                      prep x (Just xs) = Just (x:xs)


-- splitWith - a bit tortured
splitWith' :: (a -> Bool) -> [a] -> [a] -> [[a]]
splitWith' _ [] [] = []
splitWith' _ [] current = [current]
splitWith' f (x:xs) [] = if f x
                         then splitWith' f xs []
                         else splitWith' f xs [x]
splitWith' f (x:xs) current = if f x
                              then current:(splitWith' f xs [])
                              else splitWith' f xs (current ++ [x])

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = splitWith' f xs []


-- string to int conversions
asIntAcc :: String -> Int -> Int
asIntAcc [] acc = acc
asIntAcc (x:xs) acc = asIntAcc xs (digitToInt x + 10 * acc)

asInt :: String -> Int
asInt xs = asIntAcc xs 0

asIntFoldl :: String -> Int
asIntFoldl "" = error "empty string"
asIntFoldl "-" = error "minus only"
asIntFoldl ('.':_) = error "decimal point"
asIntFoldl ('-':xs) = (-1) * asIntFoldl xs
asIntFoldl xs = foldl step 0 xs
    where step acc y = acc * 10 + digitToInt y


-- own versions of Prelude functions
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x
                     then x:(takeWhile' f xs)
                     else []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr step [] xs
    where step x acc = if f x
                       then x:acc
                       else []

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr step False xs
    where step x acc = acc || f x


main = do print $ asInt "33"
          print $ asIntFoldl "3"
          print $ asIntFoldl "-33"
          print $ asIntFoldl "234"
          -- presumably an overflow here
          print $ asIntFoldl "314159265358979323846"
          print $ concat' [[1], [2], [3, 4]]
          print $ takeWhile (\x -> x < 4) [1..5]
          print $ takeWhile' (\x -> x < 4) [1..5]
          print $ takeWhile'' (\x -> x < 4) [1..5]
          print $ any' (\x -> odd x) [2, 4, 7]

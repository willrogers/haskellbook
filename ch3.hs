import Data.List


len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs


mean :: (Real a, Fractional b) => [a] -> b
mean xs = (realToFrac (sum xs)) / genericLength xs


palin :: [a] -> [a]
palin [] = []
palin (x:[]) = [x, x]
palin (x:xs) = x:(palin xs) ++ [x]


isPalin :: Eq a => [a] -> Bool
isPalin [] = True
isPalin (x:[]) = True
isPalin (x:xs) =  (x == (last xs)) && (isPalin (init xs))


intersp :: a -> [[a]] -> [a]
intersp _ [] = []
intersp _ (y:[]) = y
intersp x (y:ys) = y ++ [x] ++ (intersp x ys)



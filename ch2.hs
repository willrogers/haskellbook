
drop' :: Int -> String -> String
drop' n xs  = if n < 1 || null xs
                 then xs
                 else drop' (n - 1) (tail xs)


last' :: [a] -> a
last' [] = error "last' cannot accept empty list"
last' (x:[]) = x
last' (x:xs) = last' xs


lastButOne :: [a] -> a
lastButOne [] = error "lastButOne cannot accept empty list"
lastButOne (x:[]) = error "lastButOne cannot accept list with one item"
lastButOne (x:y:xs) = y


main = do print $ drop' 3 "hello"
          print $ last' [1, 2, 3]
          print $ lastButOne [1, 2, 3]



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


main = do print $ safeHead [1, 2, 3]
          print $ safeTail [1, 2, 3]
          print $ safeLast [1, 2, 3]
          print $ safeInit [1, 2, 3]


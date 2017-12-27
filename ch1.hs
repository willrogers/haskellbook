

file = "this is a\nlong string"

wordCount :: String -> Int
wordCount s = sum [length (words l) | l <- lines s]

charCount :: String -> Int
charCount s = length s

main = do print $ wordCount file
          print $ charCount file

import Json

maybeString :: Show a => Maybe a -> String
maybeString Nothing = ""
maybeString (Just a) = show a

maybePrint :: Show a => Maybe a -> IO ()
maybePrint Nothing = print ""
maybePrint (Just a) = print a

main = do let x = JString "hello"
          let y = JNull
          let z = JBool False
          let a = JArray [JBool False]
          let b = JObject [("true", JBool False)]
          let c = JNum 3.4
          let d = JArray [a, b, c]
          print c
          print "done"
          print $ getString x
          print $ getString y
          print $ getInt b
          print $ renderJson a
          print $ renderJson b
          print $ renderJson c
          print $ renderJson d
          print $ maybeString (Nothing::Maybe Int)
          print $ maybeString (Just 4)
          maybePrint (Nothing::Maybe Int)
          maybePrint (Just 4)

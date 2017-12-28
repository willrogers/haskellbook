module Json
    (
    JValue(..)
    , getString
    , getInt
    , renderJson
    ) where


import Data.List (intercalate)


data JValue = JString String
            | JNum Double
            | JBool Bool
            | JNull
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Ord, Show)

getString :: JValue -> (Maybe String)
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> (Maybe Int)
getInt (JNum n) = Just (truncate n)
getInt _ = Nothing

renderJson :: JValue -> String
renderJson JNull = "null"
renderJson (JString s) = s
renderJson (JBool b) = if b then "true" else "false"
renderJson (JNum n) = show n
renderJson (JArray a) = "[" ++ intercalate ", " [renderJson jv | jv <- a]  ++ "]"
renderJson (JObject o) = "{" ++ intercalate ", " [renderField f | f <- o] ++ "}"
    where renderField x = fst x ++ ": " ++ renderJson (snd x)

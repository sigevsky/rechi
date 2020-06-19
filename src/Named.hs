module Named
    ()
where

class Named a where
    name :: a -> String

type EOL = ()

instance Named String where
    name _ = "str"

instance Named Int where
    name _ = "int"

instance Named EOL where
    name _ = ""

instance (Named h, Named t) => Named (h, t) where
    name (h, t) = name h ++ " " ++ name t

some :: (String, (Int, EOL))
some = ("Hello", (3, ()))

prt :: String
prt = name some
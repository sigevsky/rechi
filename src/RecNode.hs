module RecNode where

l :: [Int]
l = [1, 2, 3, 4, 5, 6]

rev :: [a] -> Int -> Maybe [a]
rev lst n | length lst < n || n < 0 = Nothing
rev lst n = 
        let
            (l, rest) = goL lst n r
            r = goR rest []
        in Just l
    where 
        goL rest 0 racc = (racc, rest)
        goL (x:xs) i racc = goL xs (i - 1) (x:racc)  
        goL [] i racc = error "unreachble"

        goR [] acc = acc
        goR (x:xs) acc = goR xs (x:acc) 
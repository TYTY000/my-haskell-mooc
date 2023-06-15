import Data.List
palindrome :: String -> Bool
palindrome str = str == reverse str

palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1..n])


substringsOflength :: Int -> String -> [String]
substringsOflength n string = map shorten ( tails string)
      where shorten s = take n s

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k string = map tail (filter match (substringsOflength (k+1) string))
  where match sub = take 1 sub == [c]

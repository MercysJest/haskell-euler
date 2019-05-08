import Utils 

main :: IO ()
main = print $ sum $ getDigits (2^1000)

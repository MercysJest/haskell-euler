import NumberTheory
import Utils
import Data.List

main :: IO ()
main = print $ (flip (!!)) 999999 $ sort $ permutations "0123456789"

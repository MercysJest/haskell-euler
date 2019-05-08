import Utils
import NumberTheory

main :: IO ()
main = print $ sum' $ getDigits $ factorial 100

module Main where
import Data.List (maximumBy)
import Data.Ord(comparing)

-- |exercicio_1
divisivel20 :: Integer -> Bool
divisivel20 x = all (== True) [(rem x n == 0) | n <- [1..20]]

-- |exercicio_2
projectEuler5 :: Integer -> [Integer]
projectEuler5 x = [y | y <- [1..20], divisivel x y]
  where divisivel x y = rem x y == 0

-- |exercicio_3
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- |exercicio_4
projectEuler2 :: Int
projectEuler2 = sum $ filter even $ takeWhile (<4000000) $ fibs

-- |exercicio_5
exercicio5 :: [Double] -> [Double] -> Double
exercicio5 x y = sum (zipWith (*) x y)


-- |exercicio_6
collatz :: Int -> Int
collatz x
  | even x = div x 2
  | otherwise = 3*x + 1

-- |exercicio_7
collatzLen :: Int -> [Int]
collatzLen 1 = [1]
collatzLen x = x:(collatzLen proximo)
  where proximo = collatz x

-- |exercicio_8
exercicio8 :: Int
exercicio8 = maximumBy (comparing tamanho) [1..1000000]
  where tamanho n = length (collatzLen n)


main :: IO ()
main = do
  putStrLn $ "Exercicio 01 120 - " ++ show (divisivel20 120)
  putStrLn $ "Exercicio 01 2432902008176640000 - " ++ show (divisivel20 2432902008176640000)
  putStrLn $ "Exercicio 02 (35 primeiro True) - " ++ show (projectEuler5 35 !! 0)
  putStrLn $ "Exercicio 02 (35 segundo True) - " ++ show (projectEuler5 35 !! 1)
  putStrLn $ "Exercicio 03 (20 primeiros) - " ++ show (take 20 fibs)
  putStrLn $ "Exercicio 04 - " ++ show projectEuler2
  putStrLn $ "Exercicio 05 - " ++ show (exercicio5 [1, 3, -4] [1, -1, 5])
  putStrLn $ "Exercicio 06 (1) - " ++ show (collatz 1)
  putStrLn $ "Exercicio 06 (6) - " ++ show (collatz 6)
  putStrLn $ "Exercicio 07 (7) - " ++ show (collatzLen 7)
  putStrLn $ "Exercicio 07 (7) - Length = " ++ show (length (collatzLen 7))
  putStrLn $ "Exercicio 08 - " ++ show exercicio8

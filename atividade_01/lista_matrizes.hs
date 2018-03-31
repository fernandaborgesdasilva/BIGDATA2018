module Main where

-- |exercicio_1
exercicio1 :: Int -> [[Int]]
exercicio1 n = [ [fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

-- |exercicio_2
exercicio2 :: [[Int]] -> Int
exercicio2 m = sum [m !! i !! i | i <- [0.. (length m -1)]]

-- |exercicio_3
exercicio3 :: [[Int]] -> Int
exercicio3 m = sum [m !! i !! ((length m - 1) - i) | i <- [0..(length m -1)]]

main :: IO ()
main = do
  putStrLn $ "Exercicio 01 5 - " ++ show (exercicio1 5)
  putStrLn $ "Exercicio 02 [[2,1,9],[0,1,0],[1,0,7]] - " ++ show (exercicio2 [[2,1,9],[0,1,0],[1,0,7]])
  putStrLn $ "Exercicio 03 [[2,1,9],[0,1,0],[1,0,7]] - " ++ show (exercicio3 [[2,1,9],[0,1,0],[1,0,7]])

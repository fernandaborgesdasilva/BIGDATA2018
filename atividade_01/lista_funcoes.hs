module Main where

-- |exercicio_1
exercicio1 :: Int -> Int -> Int -> Bool
exercicio1 x y z
  | (x + y > z) && (x + z > y) && (y + z > x) = True
  | otherwise = False

-- |exercicio_2
exercicio2 :: Eq a => a -> a -> a -> String
exercicio2 a b c
  | ((a == b) && (b == c)) = "Equilatero"
  | ((a /= b) && (a /= c) && (b /= c)) = "Escaleno"
  | otherwise = "Isosceles"

-- |exercicio_3
exercicio3 :: Integer -> Integer -> Integer
exercicio3 1 n = n
exercicio3 m n
  | even m = exercicio3 (div m 2) (n*2)
  | otherwise = n + exercicio3 (div m 2) (n*2)

-- |exercicio_4
exercicio4 :: Integer -> Bool
exercicio4 x = null $ filter (divisivel x) [2..(x-1)]
    where divisivel x y = rem x y == 0

-- |exercicio_5
exercicio5 :: Int -> Int
exercicio5 n = sum (map (\x -> read [x] :: Int) (show n))

-- |exercicio_6
exercicio6 :: Int -> Int
exercicio6 x = pa x 0
    where
    pa x cont
      | x < 10 = cont
      | otherwise = pa (exercicio5 x) (cont + 1)

-- |exercicio_7
exercicio7 :: Integer -> Integer -> Integer
exercicio7 n k = div (product [k+1..n]) (product [1..n-k])

exercicio8 = exercicio7


main :: IO ()
main = do
  putStrLn $ "Exercicio 01 (1,2,3) - " ++ show (exercicio1 1 2 3)
  putStrLn $ "Exercicio 01 (3,4,5) - " ++ show (exercicio1 3 4 5)
  if (exercicio1 1 2 3) then putStrLn $ "Exercicio 02 (1,2,3) - " ++ show (exercicio2 1 2 3) else putStrLn $ "Exercicio 02 (1,2,3) - Nao eh um triangulo"
  if (exercicio1 3 4 5) then putStrLn $ "Exercicio 02 (3,4,5) - " ++ show (exercicio2 3 4 5) else putStrLn $ "Exercicio 02 (3,4,5) - Nao eh um triangulo"
  if (exercicio1 4 4 4) then putStrLn $ "Exercicio 02 (4,4,4) - " ++ show (exercicio2 4 4 4) else putStrLn $ "Exercicio 02 (4,4,4) - Nao eh um triangulo"
  if (exercicio1 3 4 4) then putStrLn $ "Exercicio 02 (3,4,4) - " ++ show (exercicio2 3 4 4) else putStrLn $ "Exercicio 02 (3,4,4) - Nao eh um triangulo"
  putStrLn $ "Exercicio 03 (33,40) - " ++ show (exercicio3 33 40)
  putStrLn $ "Exercicio 04 (17) - " ++ show (exercicio4 17)
  putStrLn $ "Exercicio 04 (4) - " ++ show (exercicio4 4)
  putStrLn $ "Exercicio 05 (791) - " ++ show (exercicio5 791)
  putStrLn $ "Exercicio 06 (99999999999) - " ++ show (exercicio6 99999999999)
  putStrLn $ "Exercicio 07 (5 3) - " ++ show (exercicio7 5 3)
  putStrLn $ "Exercicio 08 (10 5) - " ++ show (exercicio8 10 5)

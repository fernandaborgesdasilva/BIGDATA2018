module Main where

-- |exercicio_1
exercicio01 :: (Double, Double, Double)
exercicio01 = (2*3+5 , 2+2*3+1 , 3**4+5*2**5+1)

-- |exercicio_2
mult3 :: Int -> Bool
mult3 x = rem x 3 == 0

-- |exercicio_3
mult5 :: Int -> Bool
mult5 x = rem x 5 == 0

-- |exercicio_4
mult35 :: Int -> Bool
mult35 x = mult3 x && mult5 x

-- |exercicio_5
exercicio5 :: Int -> Bool
exercicio5 x = x < -1 || x > 1 && rem x 2 == 0

-- |exercicio_6
exercicio6 :: Integer -> Double
exercicio6 x = (fromIntegral x) / 2

-- |exercicio_7
exercicio7 :: Double -> (Double, Double)
exercicio7 x = (sqrt((1 - cos(x))/2) , -sqrt((1 - cos(x))/2))

-- |exercicio_8
bissexto a = (rem a 4 == 0 && (rem a 100 /= 0)) || rem a 400 == 0
exercicio8 :: [Int]
exercicio8 = filter bissexto [1..2018]

-- |exercicio_9
exercicio9 :: Int -> [a] -> [a]
exercicio9 n lista = drop (length lista - n) lista

-- |exercicio_10
exercicio10 :: ([Int], [Int])
exercicio10 = (take metade exercicio8 , drop metade exercicio8)
  where metade = div(length exercicio8)2

-- |exercicio_11
exercicio11 :: String -> String -> String
exercicio11 x y = x ++ (' ' : y)

-- |exercicio_12
exercicio12 :: String -> [Int]
exercicio12 string = [read [x] :: Int | x <- string]

-- |'main' executa programa principal
main :: IO ()
main = do
    putStrLn $ "Exercicio 01 - " ++ show (exercicio01)
    putStrLn $ "Exercicio 02 (21 mult3) - " ++ show (mult3 21)
    putStrLn $ "Exercicio 02 (20 mult3) - " ++ show (mult3 20)
    putStrLn $ "Exercicio 03 (21 mult5) - " ++ show (mult5 21)
    putStrLn $ "Exercicio 03 (20 mult5) - " ++ show (mult5 20)
    putStrLn $ "Exercicio 04 (20 mult35) - " ++ show (mult35 20)
    putStrLn $ "Exercicio 04 (45 mult35) - " ++ show (mult35 45)
    putStrLn $ "Exercicio 05 (-20) - " ++ show (exercicio5 (-20))
    putStrLn $ "Exercicio 05 (1) - " ++ show (exercicio5 1)
    putStrLn $ "Exercicio 05 (-1) - " ++ show (exercicio5 (-1))
    putStrLn $ "Exercicio 05 (3) - " ++ show (exercicio5 3)
    putStrLn $ "Exercicio 05 (4) - " ++ show (exercicio5 4)
    putStrLn $ "Exercicio 06 (4) - " ++ show (exercicio6 4)
    putStrLn $ "Exercicio 06 (3) - " ++ show (exercicio6 3)
    putStrLn $ "Exercicio 07 (4) - " ++ show (exercicio7 4)
    putStrLn $ "Exercicio 07 (3) - " ++ show (exercicio7 3)
    putStrLn $ "Exercicio 08 - " ++ show exercicio8
    putStrLn $ "Exercicio 09 (10 primeiros) - " ++ show (take 10 exercicio8)
    putStrLn $ "Exercicio 09 (10 ultimos) - " ++ show (exercicio9 10 exercicio8)
    putStrLn $ "Exercicio 10 - " ++ show exercicio10
    putStrLn $ "Exercicio 11 - " ++ show (exercicio11 "Ola" "mundo!")
    putStrLn $ "Exercicio 12 (string = 0123456789) - " ++ show (exercicio12 "0123456789")

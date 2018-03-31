module Main where

data Solution = Solution House House House House House deriving (Show, Eq)

data House = House {color :: Color, compatriot :: Compatriot, pet :: Pet, beverage :: Beverage, cigarette :: Cigarette } deriving (Show, Eq)

data Color = Blue | Green | Ivory | Red | Yellow
  deriving (Show, Eq)

colors :: [Color]
colors = [Blue, Green, Ivory, Red, Yellow]

data Compatriot = Englishman | Japanese | Norwegian | Spaniard | Ukrainian
  deriving (Show, Eq)

compatriots :: [Compatriot]
compatriots = [Englishman, Japanese, Norwegian, Spaniard, Ukrainian]

data Pet = Dog | Fox | Horse | Snails | Zebra
  deriving (Show, Eq)

pets :: [Pet]
pets = [Dog, Fox, Horse, Snails, Zebra]

data Beverage = Coffee | Juice | Milk | Tea | Water
  deriving (Show, Eq)

beverages :: [Beverage]
beverages = [Coffee, Juice, Milk, Tea, Water]

data Cigarette = Chesterfields | Kools | Luckies | Old_Gold | Parliaments
  deriving (Show, Eq)

cigarettes :: [Cigarette]
cigarettes = [Chesterfields, Kools, Luckies, Old_Gold, Parliaments]


--primeiro validar caracteristicas

--The Englishman lives in the red house
p1 :: House -> Bool
p1 h = (compatriot h == Englishman) == (color h == Red)

--The Spaniard owns the dog
p2 :: House -> Bool
p2 h = (compatriot h == Spaniard) == (pet h == Dog)

--Coffee is drunk in the green house
p3 :: House -> Bool
p3 h = (beverage h == Coffee) == (color h == Green)

--The Ukrainian drinks tea
p4 :: House -> Bool
p4 h = (compatriot h == Ukrainian) == (beverage h == Tea)

-- The Old Gold smoker owns snails.
p5 :: House -> Bool
p5 h = (cigarette h == Old_Gold) == (pet h == Snails)

--Kools are smoked in the yellow house.
p6 :: House -> Bool
p6 h = (cigarette h == Kools) == (color h == Yellow)

--The Lucky Strike smoker drinks orange juice
p7 :: House -> Bool
p7 h = (cigarette h == Luckies) == (beverage h == Juice)

--The Japanese smokes Parliaments
p8 :: House -> Bool
p8 h = (compatriot h == Japanese) == (cigarette h == Parliaments)

hps :: [House -> Bool]
hps = [p1, p2, p3, p4, p5, p6, p7, p8]

--depois validar ordem das casas

--Milk is drunk in the middle house.
p9 :: Solution -> Bool
p9 (Solution _ _ h3 _ _) = beverage h3 == Milk

--The Norwegian lives in the first house.
p10 :: Solution -> Bool
p10 (Solution h1 _ _ _ _) = compatriot h1 == Norwegian

--The green house is immediately to the right of the ivory house
p11 :: Solution -> Bool
p11 (Solution h1 h2 h3 h4 h5) = any p cs
  where
    p (left, right) = color right == Green && color left == Ivory
    cs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

--The man who smokes Chesterfields lives in the house next to the man with the fox
p12 :: Solution -> Bool
p12 (Solution h1 h2 h3 h4 h5) = any p cs
  where
    p (left, right) =
        (cigarette left == Chesterfields && pet right == Fox)
        || (cigarette right == Chesterfields && pet left == Fox)
    cs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

--Kools are smoked in the house next to the house where the horse is
p13 :: Solution -> Bool
p13 (Solution h1 h2 h3 h4 h5) = any p cs
  where
    p (left, right) =
        (cigarette left == Kools && pet right == Horse)
        || (cigarette right == Kools && pet left == Horse)
    cs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

-- The Norwegian lives next to the blue house
p14 :: Solution -> Bool
p14 (Solution h1 h2 h3 h4 h5) = any p cs
  where
    p (left, right) =
        (compatriot left == Norwegian && color right == Blue)
        || (compatriot right == Norwegian && color left == Blue)
    cs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

--ordenando
p15 :: Solution -> Bool
p15 (Solution h1 h2 h3 h4 h5) = all p cs
  where
    p ((House a1 b1 c1 d1 e1), (House a2 b2 c2 d2 e2)) =
        and [a1 /= a2, b1 /= b2, c1 /= c2, d1 /= d2, e1 /= e2]
    cs =
        [ (h1, h2), (h1, h3), (h1, h4), (h1, h5), (h2, h3)
        , (h2, h4), (h2, h5), (h3, h4), (h3, h5), (h4, h5)
        ]

sps :: [Solution -> Bool]
sps = [p9, p10, p11, p12, p13, p14, p15]

houseSpace :: [House]
houseSpace =
    [ House a b c d e
    | a <- colors
    , b <- compatriots
    , c <- pets
    , d <- beverages
    , e <- cigarettes
    ]

--levanta as alternativas que condizem com as caracteristicas
houses :: [House]
houses = filter (check hps) houseSpace

solutionSpace :: [Solution]
solutionSpace =
    [ (Solution h1 h2 h3 h4 h5)
    | h1 <- h, h2 <- h, h3 <- h, h4 <- h, h5 <- h ]
  where h = houses


check :: [a -> Bool] -> a -> Bool
check ps x = and $ map ($ x) ps

--levanta as alternativas que condizem a ordem
solutions :: [Solution]
solutions = filter (check sps) solutionSpace

main :: IO [()]
main = mapM print solutions

--Haskell 03
--Aluno: Lucas Caetano

add10toall :: [Int] -> [Int]
add10toall x = [(y+10) | y <- x]

multN :: Int -> [Int] -> [Int]
multN x y = [(z*x) | z <- y]

multN' :: Int -> [Int] -> [Int]
multN' z c = map ((\x y -> x*y) z) c

applyExpr :: [Int] -> [Int]
applyExpr x = [(3*z+2) | z <- x]

applyExpr' :: [Int] -> [Int]
applyExpr' x = map (\y -> 3*y+2) x 

addSuffix :: String -> [String] -> [String]
addSuffix x y = [(c++x) | c <- y]

selectgt5 :: [Int] -> [Int]
selectgt5 x = [z | z <- x, z > 5]

sumOdds :: [Int] -> Int
sumOdds x = sum [z | z <- x, odd z]

sumOdds' :: [Int] -> Int
sumOdds' x = sum (filter odd x)

selectExpr :: [Int] -> [Int]
selectExpr x = [z | z <- x, even z && z >= 20 && z <= 50]

countShorts :: [String] -> Int
countShorts x = length[z | z <- x, length z < 5]

calcExpr :: [Float] -> [Float]
calcExpr x = [(z^2/2) | z <- x, z > 10]

trSpaces :: String -> String
trSpaces x = [if z == ' ' then '-' else z | z <- x]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd x = [snd z | z <- x]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum[(snd z) * (fst z) | z <- zip x y]

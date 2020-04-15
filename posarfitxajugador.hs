import Control.Monad   
import System.Random


data Jugador = Ordinador | Participant

data Tauler_Invertit = Tauler_Invertit [[Int]]
    deriving (Show)
--I dont have the idea of printing it using show, instead we will print it looping through its rows

data Tauler = Tauler [[Int]]
    deriving (Show)
--n = files, m = columnes

--construirTauler :: Int -> Int -> Tauler
--n es el nombre de columnes
--construirTauler n m = Tauler (splitEvery m (take (n*m) (iterate (+0) 0)))

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

construirTauler :: Tauler
--n es el nombre de columnes
construirTauler = Tauler (splitEvery 6 (take (42) (iterate (+0) 0)))

splitEvery :: Int -> [Int] -> [[Int]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

substituir :: [[Int]] -> [Int] -> Int -> [[Int]]
--tauler,columna nova, num columna, tauler
substituir [] columna 0 = [columna]
substituir (xs:x) columna 0 = [columna] ++ x
substituir (xs:x) columna numero = [xs] ++ substituir x columna (numero - 1) 


notFullrow :: Tauler -> Int -> Bool
notFullrow (Tauler t1) row = 
    if (t1!!row!!((length (t1!!row))-1) /= 0) then
        False
    else
        True

posarfitxajugador :: Tauler -> Jugador -> Int -> Tauler
posarfitxajugador (Tauler a) (Ordinador) num = Tauler (substituir a (marcarPC (a!!num)) num)
    where 
        marcarPC :: [Int] -> [Int]
        marcarPC [] = []
        marcarPC (xs:x) =
            if (xs /= 0) then
                [xs] ++ marcarPC x
            else
                [(xs+1)] ++  x
--Haurem posat un 1
posarfitxajugador (Tauler a) (Participant) num = Tauler(substituir (a) (marcarUSER (a!!num)) num)
    where   
        marcarUSER :: [Int] -> [Int]
        marcarUSER [] = []
        marcarUSER (xs:x) = 
            if (xs /= 0) then
                [xs] ++ marcarUSER x
            else
                [(xs+2)] ++ x
--Greedy strategy computer
--evitar que ell posi el 4 en ratlla, i fer on nhi haura mes dimmediat
evaluarverticalamunt :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluarverticalamunt a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluarverticalamunt a Ordinador i (j+1)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluarverticalamunt a Participant i j = 
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluarverticalamunt a Participant i (j+1)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0

evaluarverticalavall :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluarverticalavall a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluarverticalavall a Ordinador i (j-1)
            --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluarverticalavall a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluarverticalavall a Participant i (j-1)
            --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0

evaluar45dreta :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluar45dreta a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar45dreta a Ordinador (i+1) (j+1)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluar45dreta a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluar45dreta a Participant (i+1) (j+1)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0


evaluar45esquerra :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluar45esquerra a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar45esquerra a Ordinador (i-1) (j-1)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluar45esquerra a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluar45esquerra a Participant (i-1) (j-1)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0

evaluahortizontaldreta :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluahortizontaldreta a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluahortizontaldreta a Ordinador (i+1) (j)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluahortizontaldreta a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluahortizontaldreta a Participant (i+1) (j)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0

evaluahortizontalesquerra :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluahortizontalesquerra a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluahortizontalesquerra a Ordinador (i-1) (j)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluahortizontalesquerra a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluahortizontalesquerra a Participant (i-1) (j)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0

evaluar315dreta :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluar315dreta a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar315dreta a Ordinador (i+1) (j-1)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluar315dreta a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluar315dreta a Participant (i+1) (j-1)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0

evaluar315esquerra :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluar315esquerra a Ordinador i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar315esquerra a Ordinador (i-1) (j+1)
        else
            if (a!!i!!j == 2) then
                -1000000
            else
                0
    else
        0
evaluar315esquerra a Participant i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 2) then
            1 + evaluar315esquerra a Participant (i-1) (j+1)
        else
            if (a!!i!!j == 1) then
                -1000000
            else
                0
    else
        0





prova :: [[Int]] -> Jugador -> Int -> Int -> [Int]
prova a Ordinador i j = 
    if (a!!i!!j == 1) then
        ([evaluarverticalamunt a Ordinador i (j+1) + evaluarverticalavall a Ordinador i (j-1)] ++ [evaluar45dreta a  Ordinador (i+1)(j+1)
            + evaluar45esquerra a  Ordinador (i-1)(j-1)] ++ [evaluahortizontaldreta a  Ordinador (i+1)(j) + evaluahortizontalesquerra a  Ordinador (i-1)(j)] ++
            [evaluar315dreta a  Ordinador (i+1)(j-1) + evaluar315esquerra a  Ordinador (i-1)(j+1)] ++ [evaluarverticalamunt a Ordinador i (j+1)]
            ++ [evaluarverticalavall a Ordinador i (j-1)] ++ [evaluar45dreta a  Ordinador (i+1)(j+1)] ++ [evaluar45esquerra a  Ordinador (i-1)(j-1)]
            ++ [evaluahortizontaldreta a  Ordinador (i+1)(j)] ++ [ evaluahortizontalesquerra a  Ordinador (i-1)(j)] ++
            [evaluar315dreta a  Ordinador (i+1)(j-1)] ++ [evaluar315esquerra a  Ordinador (i-1)(j+1)] ) 
    else
        [0]
prova a Participant i j = 
    if (a!!i!!j == 2) then
        ([evaluarverticalamunt a Participant i (j+1) + evaluarverticalavall a Participant i (j-1)] ++ [evaluar45dreta a  Participant (i+1)(j+1)
            + evaluar45esquerra a  Participant (i-1)(j-1)] ++ [evaluahortizontaldreta a  Participant (i+1)(j) + evaluahortizontalesquerra a  Participant (i-1)(j)] ++
            [evaluar315dreta a  Participant (i+1)(j-1) + evaluar315esquerra a  Participant (i-1)(j+1)] ++ [evaluarverticalamunt a Participant i (j+1)]
            ++ [evaluarverticalavall a Participant i (j-1)] ++ [evaluar45dreta a  Participant (i+1)(j+1)] ++ [evaluar45esquerra a  Participant (i-1)(j-1)]
            ++ [evaluahortizontaldreta a  Participant (i+1)(j)] ++ [ evaluahortizontalesquerra a  Participant (i-1)(j)] ++
            [evaluar315dreta a  Participant (i+1)(j-1)] ++ [evaluar315esquerra a  Participant (i-1)(j+1)] ) 
    else
        [0]





evaluarposicio :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluarposicio a Ordinador i j = 
    if (a!!i!!j == 1) then
        1 + maximum ([evaluarverticalamunt a Ordinador i (j+1) + evaluarverticalavall a Ordinador i (j-1)] ++ [evaluar45dreta a  Ordinador (i+1)(j+1)
            + evaluar45esquerra a  Ordinador (i-1)(j-1)] ++ [evaluahortizontaldreta a  Ordinador (i+1)(j) + evaluahortizontalesquerra a  Ordinador (i-1)(j)] ++
            [evaluar315dreta a  Ordinador (i+1)(j-1) + evaluar315esquerra a  Ordinador (i-1)(j+1)] ++ [evaluarverticalamunt a Ordinador i (j+1)]
            ++ [evaluarverticalavall a Ordinador i (j-1)] ++ [evaluar45dreta a  Ordinador (i+1)(j+1)] ++ [evaluar45esquerra a  Ordinador (i-1)(j-1)]
            ++ [evaluahortizontaldreta a  Ordinador (i+1)(j)] ++ [ evaluahortizontalesquerra a  Ordinador (i-1)(j)] ++
            [evaluar315dreta a  Ordinador (i+1)(j-1)] ++ [evaluar315esquerra a  Ordinador (i-1)(j+1)] ) 
    else
        0
evaluarposicio a Participant i j = 
    if (a!!i!!j == 2) then
        1 + maximum([evaluarverticalamunt a Participant i (j+1) + evaluarverticalavall a Participant i (j-1)] ++ [evaluar45dreta a  Participant (i+1)(j+1)
            + evaluar45esquerra a  Participant (i-1)(j-1)] ++ [evaluahortizontaldreta a  Participant (i+1)(j) + evaluahortizontalesquerra a  Participant (i-1)(j)] ++
            [evaluar315dreta a  Participant (i+1)(j-1) + evaluar315esquerra a  Participant (i-1)(j+1)] ++ [evaluarverticalamunt a Participant i (j+1)]
            ++ [evaluarverticalavall a Participant i (j-1)] ++ [evaluar45dreta a  Participant (i+1)(j+1)] ++ [evaluar45esquerra a  Participant (i-1)(j-1)]
            ++ [evaluahortizontaldreta a  Participant (i+1)(j)] ++ [ evaluahortizontalesquerra a  Participant (i-1)(j)] ++
            [evaluar315dreta a  Participant (i+1)(j-1)] ++ [evaluar315esquerra a  Participant (i-1)(j+1)] ) 
    else
        0

evaluarmatriu :: [[Int]] -> Jugador -> Int -> Int -> [Int]
        --sha dinterar per les i,j --> retorna la llista i daquesta llista nhauriem dextreure el maxim a dalt
evaluarmatriu a j1 i j = 
    if (i == length(a)) then
        []
    else
        if (j == (length(a!!0)-1))then
            [evaluarposicio a j1 i j] ++ evaluarmatriu a j1 (i+1) (0)
            else
            [evaluarposicio a j1 i j] ++ evaluarmatriu a j1 i (j+1)
        

evaluarTauler :: Tauler -> Jugador -> Int
--ens ha de retornar per cada tauler quantes consequtives del jugador Ordinador hi ha
--Ordinador te la fitxa==1. he de mirar per cada 1 la seva esquerra, dreta,dalt,baix,diagonals, ho podem fer accedint i mirant si hi ha un 1
evaluarTauler (Tauler a) j1  = maximum $ evaluarmatriu a j1 0 0
--Ens quedem amb el maxim possible donat un tauler i un jugador donat

greedy_1 :: Tauler -> Jugador -> Int -> [Int]
greedy_1 (Tauler a) j1 i = 
    if (i == length(a)) then
        []
    else
        if (notFullrow (Tauler a) i) then
            [evaluarTauler (posarfitxajugador (Tauler a) j1 i) j1] ++ (greedy_1 (Tauler a) j1 (i+1))
        else
            [-10000] ++ (greedy_1 (Tauler a) j1 (i+1))
--Maybe greedy is the one that compares directly without having to receive the argument Jugador     
--First we'll see if there's an option for the contester to score 4 next round.
--If there's an option for the Participant to score 4 next round, we should aim for that row
greedy :: Tauler -> Int
greedy (Tauler x) = 
    if (fst(maximum([q | y <- [0..(length(x!!0) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)])) >= 4) then
        snd(maximum([q | y <- [0..(length(x!!0) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)]))
    else
        snd(maximum([q | y <- [0..(length(x!!0)-1)], let q = ((greedy_1 (Tauler x) Ordinador 0)!!y,y)]))
--no es res fer que fer una argmax

minmaxarray :: Tauler -> [(Int,Int)]
minmaxarray (Tauler x) = ([(maximum(greedy_1 z1 Participant 0),z) | z <- [0..(length(x)-1)], 
    let z1 = posarfitxajugador (Tauler x) Ordinador z])

minmax :: Tauler -> Int
minmax (Tauler x) = snd(minimum[(maximum(greedy_1 z1 Participant 0),z) | z <- [0..(length(x)-1)], 
    let z1 = posarfitxajugador (Tauler x) Ordinador z])
--It is not exactly the transpose I am looking for tbh, but the name works
transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = []
transpose  x = (map last x) : transpose (map init x)

caracters = \x -> 
    if (x == 1) then 'X'
    else
        if (x == 2) then 'O'
        else
            '-'

escriutauler :: [[Int]] -> IO()
escriutauler [] = do 
    putStrLn("")
escriutauler (xs:x) = do 
    print(map caracters xs)
    escriutauler x

extreutauler :: Tauler -> [[Int]]
extreutauler (Tauler t1) = t1

jugar :: Tauler -> Int -> IO ()
jugar (Tauler t1) level
    | level == 1 =
        do
            print $ evaluarmatriu (t1) Participant 0 0
            if ((evaluarTauler (Tauler t1) Participant) >= 4) then do
                putStrLn("You won!")
                escriutauler (transpose t1)

            else
                if ((evaluarTauler (Tauler t1) Ordinador) >= 4) then do
                    putStrLn("The machine beat you!")
                    escriutauler (transpose t1)
                else do
                    escriutauler (transpose t1)
                    putStrLn("Introdueix la columna on voldries posar la fitxa, tu ets '0'")
                    num <- getLine
                    let taulernou = (posarfitxajugador (Tauler t1) Participant (fromEnum (num!!0)-48))
                    numero <- randInt 0 6
                    jugar (posarfitxajugador taulernou Ordinador numero) level
    | level == 2 =
        do
            print $ evaluarmatriu (t1) Participant 0 0
            if ((evaluarTauler (Tauler t1) Participant) >= 4) then do
                putStrLn("You won!")
                escriutauler (transpose t1)

            else
                if ((evaluarTauler (Tauler t1) Ordinador) >= 4) then do
                    putStrLn("The machine beat you!")
                    escriutauler (transpose t1)
                else do
                    escriutauler (transpose t1)
                    putStrLn("Introdueix la columna on voldries posar la fitxa, tu ets '0'")
                    num <- getLine
                    let taulernou = (posarfitxajugador (Tauler t1) Participant (fromEnum (num!!0)-48))
                    jugar (posarfitxajugador taulernou Ordinador (greedy taulernou)) level
    | level == 3 =
        do
            print $ evaluarmatriu (t1) Participant 0 0
            if ((evaluarTauler (Tauler t1) Participant) >= 4) then do
                putStrLn("You won!")
                escriutauler (transpose t1)

            else
                if ((evaluarTauler (Tauler t1) Ordinador) >= 4) then do
                    putStrLn("The machine beat you!")
                    escriutauler (transpose t1)
                else do
                    escriutauler (transpose t1)
                    putStrLn("Introdueix la columna on voldries posar la fitxa, tu ets '0'")
                    num <- getLine
                    let taulernou = (posarfitxajugador (Tauler t1) Participant (fromEnum (num!!0)-48))
                    let prova = (extreutauler taulernou)
                    escriutauler $ transpose $ prova
                    print (minmaxarray taulernou)
                    jugar (posarfitxajugador taulernou Ordinador (minmax taulernou)) level

    |otherwise = putStrLn("Undefined Level!")

main = do
    putStrLn("Which level do you wanna face? 1-Easy, 2-Medium, 3-Hard")
    level <- getLine
    putStrLn("Introdueix la columna on vols posar la fitxa! Tu Ets ''X''")
    let y = construirTauler in jugar y (fromEnum (level!!0)-48)
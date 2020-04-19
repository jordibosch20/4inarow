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

construirTauler :: Int -> Int -> Tauler
--n es el nombre de columnes
construirTauler n m = Tauler (splitEvery n (take (m*n) (iterate (+0) 0)))

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
evaluarverticalamunt :: [[Int]] -> Bool -> Jugador -> Int -> Int -> Int
evaluarverticalamunt a valor Ordinador i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluarverticalamunt a valor Ordinador i (j+1)
            else
                0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluarverticalamunt a valor Ordinador i (j+1)
            else
                if (a!!i!!j == 2) then
                    -1000000
                else
                    0   
        else
            0
evaluarverticalamunt a valor Participant i j =
    if (valor) then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluarverticalamunt a valor Participant i (j+1)
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluarverticalamunt a valor Participant i (j+1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0

evaluarverticalavall :: [[Int]] -> Bool  -> Jugador -> Int -> Int -> Int
evaluarverticalavall a valor Ordinador i j =
    if (valor) then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluarverticalavall a valor Ordinador i (j-1)
                --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluarverticalavall a valor Ordinador i (j-1)
                --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
            else
                if (a!!i!!j == 2) then
                    -1000000
                else
                    0
        else
            0
evaluarverticalavall a valor Participant i j =
    if (valor) then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluarverticalavall a valor Participant i (j-1)
                --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluarverticalavall a valor Participant i (j-1)
                --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0

evaluar45dreta :: [[Int]] -> Bool  -> Jugador -> Int -> Int -> Int
evaluar45dreta a valor Ordinador i j =
    if (valor) then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar45dreta a  valor Ordinador (i+1) (j+1)
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar45dreta a  valor Ordinador (i+1) (j+1)
            else
                if (a!!i!!j == 2) then
                    -1000000
                else
                    0
        else
            0
evaluar45dreta a valor Participant i j =
    if (valor) then 
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar45dreta a  valor Participant (i+1) (j+1)
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar45dreta a valor Participant (i+1) (j+1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0

evaluar45esquerra :: [[Int]] -> Bool  -> Jugador -> Int -> Int -> Int
evaluar45esquerra a valor Ordinador i j =
    if (valor) then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar45esquerra a valor Ordinador (i-1) (j-1)
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar45esquerra a valor Ordinador (i-1) (j-1)
            else
                if (a!!i!!j == 2) then
                    -1000000
                else
                    0
    else
        0
evaluar45esquerra a valor Participant i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar45esquerra a valor Participant (i-1) (j-1)
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar45esquerra a valor Participant (i-1) (j-1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
    else
        0
evaluahortizontaldreta :: [[Int]] -> Bool  -> Jugador -> Int -> Int -> Int
evaluahortizontaldreta a valor Ordinador i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluahortizontaldreta a valor Ordinador (i+1) (j)
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluahortizontaldreta a valor Ordinador (i+1) (j)
            else
                if (a!!i!!j == 2) then
                    -1000000
                else
                    0
        else
            0
evaluahortizontaldreta a valor Participant i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluahortizontaldreta a valor Participant (i+1) (j)
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluahortizontaldreta a valor Participant (i+1) (j)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0
evaluahortizontalesquerra :: [[Int]] -> Bool  -> Jugador -> Int -> Int -> Int
evaluahortizontalesquerra a valor Ordinador i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluahortizontalesquerra a valor Ordinador (i-1) (j)
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluahortizontaldreta a valor Participant (i+1) (j)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0
evaluahortizontalesquerra a valor Participant i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluahortizontalesquerra a valor Participant (i-1) (j)
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluahortizontalesquerra a valor Participant (i-1) (j)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0

evaluar315dreta :: [[Int]] -> Bool -> Jugador -> Int -> Int -> Int
evaluar315dreta a valor Ordinador i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar315dreta a valor Ordinador (i+1) (j-1)
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar315dreta a valor Ordinador (i+1) (j-1)
            else
                if (a!!i!!j == 2) then
                    -1000000
                else
                    0
    else
        0
evaluar315dreta a valor Participant i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar315dreta a valor Participant (i+1) (j-1)
            else
                if (a!!i!!j == 1) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar315dreta a valor Participant (i+1) (j-1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0


evaluar315esquerra :: [[Int]] -> Bool  -> Jugador -> Int -> Int -> Int
evaluar315esquerra a valor Ordinador i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 1) then
                1 + evaluar315esquerra a valor Ordinador (i-1) (j+1)
            else
                if (a!!i!!j == 2) then
                    0
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar315dreta a valor Participant (i+1) (j-1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0
evaluar315esquerra a valor Participant i j =
    if valor then
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar315esquerra a valor Participant (i-1) (j+1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
        else
            0
    else
        if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
            if (a!!i!!j == 2) then
                1 + evaluar315dreta a valor Participant (i+1) (j-1)
            else
                if (a!!i!!j == 1) then
                    -1000000
                else
                    0
    else
        0




evaluarposicio :: [[Int]] -> Jugador -> Int -> Int -> Int
evaluarposicio a Ordinador i j = 
    if (a!!i!!j == 1) then
        if ( 1 + maximum ([evaluarverticalamunt a True Ordinador i (j+1) + evaluarverticalavall a True Ordinador i (j-1)] ++ [evaluar45dreta a True  Ordinador (i+1)(j+1)
            + evaluar45esquerra a True  Ordinador (i-1)(j-1)] ++ [evaluahortizontaldreta a True  Ordinador (i+1)(j) + evaluahortizontalesquerra a True  Ordinador (i-1)(j)] ++
            [evaluar315dreta a True  Ordinador (i+1)(j-1) + evaluar315esquerra a True  Ordinador (i-1)(j+1)] ++ [evaluarverticalamunt a True Ordinador i (j+1)]
            ++ [evaluarverticalavall a True Ordinador i (j-1)] ++ [evaluar45dreta a True  Ordinador (i+1)(j+1)] ++ [evaluar45esquerra a True  Ordinador (i-1)(j-1)]
            ++ [evaluahortizontaldreta a True  Ordinador (i+1)(j)] ++ [ evaluahortizontalesquerra a  True Ordinador (i-1)(j)] ++
            [evaluar315dreta a  True Ordinador (i+1)(j-1)] ++ [evaluar315esquerra a  True Ordinador (i-1)(j+1)] ) >= 4 ) then
            4
        else
            1 + maximum ([evaluarverticalamunt a False Ordinador i (j+1) + evaluarverticalavall a False Ordinador i (j-1)] ++ [evaluar45dreta a False  Ordinador (i+1)(j+1)
                + evaluar45esquerra a False  Ordinador (i-1)(j-1)] ++ [evaluahortizontaldreta a  False Ordinador (i+1)(j) + evaluahortizontalesquerra a False  Ordinador (i-1)(j)] ++
                [evaluar315dreta a  False Ordinador (i+1)(j-1) + evaluar315esquerra a  False Ordinador (i-1)(j+1)] ++ [evaluarverticalamunt a  False Ordinador i (j+1)]
                ++ [evaluarverticalavall a False Ordinador i (j-1)] ++ [evaluar45dreta a False Ordinador (i+1)(j+1)] ++ [evaluar45esquerra a  False  Ordinador (i-1)(j-1)]
                ++ [evaluahortizontaldreta a False  Ordinador (i+1)(j)] ++ [ evaluahortizontalesquerra a  False Ordinador (i-1)(j)] ++
                [evaluar315dreta a  False Ordinador (i+1)(j-1)] ++ [evaluar315esquerra a  False Ordinador (i-1)(j+1)] ) 
    else
        0
evaluarposicio a Participant i j = 
    if (a!!i!!j == 2) then
        if (1 + maximum([evaluarverticalamunt a True Participant i (j+1) + evaluarverticalavall a True Participant i (j-1)] ++ [evaluar45dreta a True  Participant (i+1)(j+1)
                + evaluar45esquerra a  True Participant (i-1)(j-1)] ++ [evaluahortizontaldreta a True  Participant (i+1)(j) + evaluahortizontalesquerra a  True Participant (i-1)(j)] ++
                [evaluar315dreta a  True Participant (i+1)(j-1) + evaluar315esquerra a  True Participant (i-1)(j+1)] ++ [evaluarverticalamunt a True Participant i (j+1)]
                ++ [evaluarverticalavall a True Participant i (j-1)] ++ [evaluar45dreta a True  Participant (i+1)(j+1)] ++ [evaluar45esquerra a True  Participant (i-1)(j-1)]
                ++ [evaluahortizontaldreta a True  Participant (i+1)(j)] ++ [ evaluahortizontalesquerra a True  Participant (i-1)(j)] ++
                [evaluar315dreta a True  Participant (i+1)(j-1)] ++ [evaluar315esquerra a True  Participant (i-1)(j+1)] ) >= 4) then
            4
        else    
            1 + maximum([evaluarverticalamunt a False Participant i (j+1) + evaluarverticalavall a False Participant i (j-1)] ++ [evaluar45dreta a False  Participant (i+1)(j+1)
                + evaluar45esquerra a  False Participant (i-1)(j-1)] ++ [evaluahortizontaldreta a  False Participant (i+1)(j) + evaluahortizontalesquerra a  False Participant (i-1)(j)] ++
                [evaluar315dreta a False  Participant (i+1)(j-1) + evaluar315esquerra a  False Participant (i-1)(j+1)] ++ [evaluarverticalamunt a False Participant i (j+1)]
                ++ [evaluarverticalavall a False Participant i (j-1)] ++ [evaluar45dreta a  False Participant (i+1)(j+1)] ++ [evaluar45esquerra a False  Participant (i-1)(j-1)]
                ++ [evaluahortizontaldreta a  False Participant (i+1)(j)] ++ [ evaluahortizontalesquerra a False  Participant (i-1)(j)] ++
                [evaluar315dreta a  False Participant (i+1)(j-1)] ++ [evaluar315esquerra a False  Participant (i-1)(j+1)] ) 
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

evaluarTaulerprova :: Tauler -> Jugador -> [Int]
evaluarTaulerprova (Tauler a) j1  =  evaluarmatriu a j1 0 0


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
    if (fst(maximum([q | y <- [0..(length(x) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)])) >= 4) then
        snd(maximum([q | y <- [0..(length(x) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)]))
    else
        snd(maximum([q | y <- [0..(length(x)-1)], let q = ((greedy_1 (Tauler x) Ordinador 0)!!y,y)]))
--no es res fer que fer una argmax

--minmaxarray :: Tauler -> [(Int,Int)]
--minmaxarray (Tauler x) = [(evaluarTauler z3,z) | z <- [0..(length(x)-1)]]

mapcalcularmaxim :: [Tauler] -> [Int]
mapcalcularmaxim [] = []
mapcalcularmaxim (xs:x) = [evaluarTauler xs Participant] ++ mapcalcularmaxim x

minmax :: Tauler -> Int
minmax (Tauler x) = 
    snd(minimum[(maximum(mapcalcularmaxim (computar (Tauler x) l1)),l1) | l1 <- [0..(length(x)-1)]]) 
    where
        computar :: Tauler -> Int -> [Tauler]
        --la funcio computar iterara amb la primera component fixada
        computar (Tauler x) i = [z3 | l2 <- [0..(length(x)-1)], l3 <- [0..(length(x)-1)], let z3 = posarfitxajugador (posarfitxajugador ((posarfitxajugador (Tauler x) Ordinador i)) Participant l2) Participant l3]


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
                    numero <- randInt 0 (length(t1)-1)
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
            print (evaluarTaulerprova (Tauler t1) Participant)
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
                    let provarr = (extreutauler taulernou)
                    escriutauler $ transpose $ provarr
                    --print (minmaxarray taulernou)
                    jugar (posarfitxajugador taulernou Ordinador (minmax taulernou)) level

    |otherwise = putStrLn("Undefined Level!")

main = do
    putStrLn("Enter the number of rows")
    n <- getLine
    putStrLn("Enter the number of columns")
    m <- getLine
    putStrLn("Which level do you wanna face? 1-Easy, 2-Medium, 3-Hard")
    level <- getLine
    putStrLn("Introdueix la columna on vols posar la fitxa (comencant pel 0). Tu Ets el simbol''X''")
    let y = construirTauler (fromEnum (n!!0)-48) (fromEnum (m!!0)-48) in jugar y (fromEnum (level!!0)-48)
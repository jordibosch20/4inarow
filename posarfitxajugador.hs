import Control.Monad   

data Jugador = Ordinador | Participant

data Tauler = Tauler [[Int]]
    deriving (Show)
--n = files, m = columnes

--construirTauler :: Int -> Int -> Tauler
--n es el nombre de columnes
--construirTauler n m = Tauler (splitEvery m (take (n*m) (iterate (+0) 0)))

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
substituir (xs:x) columna 0 = [columna] ++ x
substituir (xs:x) columna numero = [xs] ++ substituir x columna (numero - 1) 

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
evaluarverticalamunt :: [[Int]] -> Int -> Int -> Int
evaluarverticalamunt a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluarverticalamunt a i (j+1)
        else
            0
    else
        0

evaluarverticalavall :: [[Int]] -> Int -> Int -> Int
evaluarverticalavall a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluarverticalavall a i (j-1)
            --AIXO PETA PERQUE VA SALTANT AMUNT I AVALL
        else
            0
    else
        0

evaluar45dreta :: [[Int]] -> Int -> Int -> Int
evaluar45dreta a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar45dreta a (i+1) (j+1)
        else
            0
    else
        0

evaluar45esquerra :: [[Int]] -> Int -> Int -> Int
evaluar45esquerra a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar45esquerra a (i-1) (j-1)
        else
            0
    else
        0

evaluahortizontaldreta :: [[Int]] -> Int -> Int -> Int
evaluahortizontaldreta a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluahortizontaldreta a (i+1) (j)
        else
            0
    else
        0

evaluahortizontalesquerra :: [[Int]] -> Int -> Int -> Int
evaluahortizontalesquerra a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluahortizontalesquerra a (i-1) (j)
        else
            0
    else
        0

evaluar315dreta :: [[Int]] -> Int -> Int -> Int
evaluar315dreta a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar315dreta a (i+1) (j-1)
        else
            0
    else
        0

evaluar315esquerra :: [[Int]] -> Int -> Int -> Int
evaluar315esquerra a i j =
    if (i >=0 && j >= 0 && i < length(a) && j < length(a!!0)) then
        if (a!!i!!j == 1) then
            1 + evaluar315esquerra a (i-1) (j-1)
        else
            0
    else
        0

evaluarposicio :: [[Int]] -> Int -> Int -> Int
evaluarposicio a i j = 
    if (a!!i!!j == 1) then
        1 + maximum ([evaluarverticalamunt a i (j+1) + evaluarverticalavall a i (j-1)] ++ [evaluar45dreta a (i+1)(j+1)
            + evaluar45esquerra a (i-1)(j-1)] ++ [evaluahortizontaldreta a (i+1)(j) + evaluahortizontalesquerra a (i-1)(j)] ++
            [evaluar315dreta a (i+1)(j-1) + evaluar315esquerra a (i-1)(j+1)]) 
    else
        0

evaluarTauler :: Tauler -> Int
--ens ha de retornar per cada tauler quantes consequtives del jugador Ordinador hi ha
--Ordinador te la fitxa==1. he de mirar per cada 1 la seva esquerra, dreta,dalt,baix,diagonals, ho podem fer accedint i mirant si hi ha un 1
evaluarTauler (Tauler a) =  maximum (evaluarmatriu a 0 0)    
    where
        evaluarmatriu :: [[Int]] -> Int -> Int -> [Int]
        --sha dinterar per les i,j --> retorna la llista i daquesta llista nhauriem dextreure el maxim a dalt
        evaluarmatriu a i j = 
            if (i == length(a)) then
                []
            else
                if (j == (length(a!!0)-1))then
                    [evaluarposicio a i j] ++ evaluarmatriu a (i+1) (0)
                else
                    [evaluarposicio a i j] ++ evaluarmatriu a i (j+1)
                
greedy :: Tauler -> Int
--Ens retorna la columna que hem de moure per aconseguir el maxim
--intentar totes les columnes aviam que ens surt
greedy x = snd(maximum([q | y <- [0..(length(greedy_1 x 0)-1)], let q = ((greedy_1 x 0)!!y,y)]))
--no es res fer que fer una argmax
    where
        greedy_1 :: Tauler -> Int -> [Int]
        greedy_1 (Tauler a) i = 
            if (i == length(a)) then
                []
            else
                [evaluarTauler (posarfitxajugador (Tauler a) (Ordinador) i)] ++ (greedy_1 (Tauler a) (i+1))

jugar :: Tauler -> IO ()
jugar t1 = do
    print(t1)
    putStrLn("Introdueix la columna on voldries posar la fitxa")
    num <- getLine
    let taulernou = (posarfitxajugador t1 Participant (fromEnum (num!!0)-48))
    jugar $ posarfitxajugador taulernou Ordinador (greedy taulernou) 

main = do
    --putStrLn("Introdueix les dimensions")
{-    n <- getChar
    espai <- getChar
    m <- getChar

    dimensions <- getLine
    if (length(dimensions) >= 2) then do
        let y = construirTauler ((fromEnum (dimensions!!0))-48) (fromEnum(dimensions!!2)-48)
        print(y)
-}
    putStrLn("Introdueix la columna on vols posar la fitxa")
    let y = construirTauler in jugar y
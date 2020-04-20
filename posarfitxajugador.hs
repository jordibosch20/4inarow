import Control.Monad   
import System.Random

data Jugador = Ordinador | Participant
--Estructura de dades que nomes pot tenir dos valors

data Tauler = Tauler [[Int]]
    deriving (Show)
--Tauler no es res mes que una llista d'Ints. Representarem els dos jugadors amb 0 i 1.

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

construirTauler :: Int -> Int -> Tauler
--n es el nombre de columnes. Donats dos enters ens contrueix un Tauler
construirTauler n m = Tauler (splitEvery n (take (m*n) (iterate (+0) 0)))

splitEvery :: Int -> [Int] -> [[Int]]
--Funcio auxiliar per crear el tauler
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

substituir :: [[Int]] -> [Int] -> Int -> [[Int]]
--Donat la matriu que representa a tauler, una columna nova, i un numero, retorna tauler amb la nova columna a la posicio indicada
substituir [] columna 0 = [columna]
substituir (xs:x) columna 0 = [columna] ++ x
substituir (xs:x) columna numero = [xs] ++ substituir x columna (numero - 1) 


notFullrow :: Tauler -> Int -> Bool
--Funcio que ens indica si donat un tauler i el numero d'una columna, aquesta esta plena de "fitxes"
--o encara hi queden llocs
notFullrow (Tauler t1) row = 
    if (t1!!row!!((length (t1!!row))-1) /= 0) then
        False
    else
        True

posarfitxajugador :: Tauler -> Jugador -> Int -> Tauler
--Funcio que donat un Tauler, un Jugador i numero on vol posar la fitxa, et retorna el nou tauler
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


evaluarverticalamunt :: [[Int]] -> Bool -> Jugador -> Int -> Int -> Int
--Funcio que donat el Tauler, un Jugador i una posicio (dos enters), ens diu quantes fitxes del jugador hi ha 
--"en ratlla" mirant cap amunt. La necessitat de la variable Bool l'explicare en detall en el read.me
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
--Funcio que donat el Tauler, un Jugador i una posicio (dos enters), ens diu quantes fitxes del jugador hi ha 
--"en ratlla" mirant cap avall. La necessitat de la variable Bool l'explicare en detall en el read.me
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
--Funcio que donat el Tauler, un Jugador i una posicio (dos enters), ens diu quantes fitxes del jugador hi ha 
--"en ratlla" mirant 45 graus a la dreta. La necessitat de la variable Bool l'explicare en detall en el read.me
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
--Funcio que evalua la linia de 45 graus cap a l'esquerra, per mirar quantes fitxes consequtives
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
--Funcio que donat el Tauler, un Jugador i una posicio (dos enters), ens diu quantes fitxes del jugador hi ha 
--"en ratlla" horitzontal a la dreta. La necessitat de la variable Bool l'explicare en detall en el read.me
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
--Funcio que donat el Tauler, un Jugador i una posicio (dos enters), ens diu quantes fitxes del jugador hi ha 
--"en ratlla" horitzontal a l'esquerra. La necessitat de la variable Bool l'explicare en detall en el read.me
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
--Funcio que evalua la linia de 315 graus cap a la dreta (-45 graus), per mirar quantes fitxes consequtives
-- hi ha en aquesta direccio
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
--Funcio que evalua la linia de 315 graus cap a la dreta (-45 graus), per mirar quantes fitxes consequtives
--hi ha en aquesta direccio
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
--Funcio que donat un tauler, un Jugador i una posicio inicial et retorna el
--nombre de fitxes consequtives mes llarg que passa per aquest lloc 
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
--Fa el mateix que evaluarposicio pero et retorna la llista amb totes 
--les posicions evaluades
evaluarmatriu a j1 i j = 
    if (i == length(a)) then
        []
    else
        if (j == (length(a!!0)-1))then
            [evaluarposicio a j1 i j] ++ evaluarmatriu a j1 (i+1) (0)
            else
            [evaluarposicio a j1 i j] ++ evaluarmatriu a j1 i (j+1)

evaluarTaulerprova :: Tauler -> Jugador -> [Int]
--Funcio que donat un tauler i un jugador, retorna la matriu de evaluarposicions
evaluarTaulerprova (Tauler a) j1  =  evaluarmatriu a j1 0 0


evaluarTauler :: Tauler -> Jugador -> Int
--Funcio que donat un tauler i un Jugador, ens retorna quin es el nombre
--maxim de fitxes que te consequtives en algun lloc del tauler
evaluarTauler (Tauler a) j1  = maximum $ evaluarmatriu a j1 0 0
--Ens quedem amb el maxim possible donat un tauler i un jugador donat

greedy_1 :: Tauler -> Jugador -> Int -> [Int]
--Donat un Tauler, un Jugador i una posicio inicial, ens retorna el resultat
--d'evaluar el nou Tauler que quedaria si posesim una fitxa a la posicio donada
greedy_1 (Tauler a) j1 i = 
    if (i == length(a)) then
        []
    else
        if (notFullrow (Tauler a) i) then
            [evaluarTauler (posarfitxajugador (Tauler a) j1 i) j1] ++ (greedy_1 (Tauler a) j1 (i+1))
        else
            [-10000] ++ (greedy_1 (Tauler a) j1 (i+1))

greedy :: Tauler -> Int
--greedy es una Funcio que el que fa es retornar donat un Tauler, on l'Ordinador hauria
--de posar la seguent fitxa seguint la seguent estrategia

--En cas que el Participant pugui guanyar a la seguent ronda, coloquem la fitxa intentant evitarho
--En cas que el Participant no pugui guanyar a la seguent ronda, coloquem la fitxa maximitzant el nombre
--de fitxes consequtives que podem tenir

--La tornare a explicar amb detall en el read.me
greedy (Tauler x) = 
    if (fst(maximum([q | y <- [0..(length(x) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)])) >= 4) then
        snd(maximum([q | y <- [0..(length(x) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)]))
--L'expressio de dalt no es res fer que fer una argmax, per trobar a quina columna hem de posar la fitxa
    else
        snd(maximum([q | y <- [0..(length(x)-1)], let q = ((greedy_1 (Tauler x) Ordinador 0)!!y,y)]))


mapcalcularmaxim :: [Tauler] -> [Int]
--Funcio que donada una llista de Taulers, els evalua tots i retorna el resultat com una llista [Int]
mapcalcularmaxim [] = []
mapcalcularmaxim (xs:x) = [evaluarTauler xs Participant] ++ mapcalcularmaxim x

minmax :: Tauler -> Int
--Funcio que fa servir un minmax per l'estrategia Smart. L'explicare amb detall al read.me
minmax (Tauler x) = 
    if (fst(maximum[q | y <- [0..(length(x) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)]) == 4) then
        --L'oponent te l'oportunitat de guanyar-nos en el seguent moviment
        snd(maximum([q | y <- [0..(length(x) -1)], let q = ((greedy_1 (Tauler x) Participant 0)!!y,y)]))
        --Fem un argmax per intentar que l'oponent no en pugui fer 4
    else
        snd(minimum[(maximum(mapcalcularmaxim (computar (Tauler x) l1)),l1) | l1 <- [0..(length(x)-1)]])
        --Fem un min-max, agafant el maxim com el resultat que el Participant pot aconseguir
        --en 2 turnos, tenint en compte que l'Ordinador jugara seguint un greedy
        where
            computar :: Tauler -> Int -> [Tauler]
            --la funcio computar donat un tauler i la posiico inicial on l'ordinador posara
            --la fitxa, ens retorna tot el conjunt de taulers possibles que s'obtindran si
            --el participant posa una fitxa, l'ordinador respon actuant greediment i el participant
            --torna a posar una fitxa
            computar (Tauler x) i = [z4 | l2 <- [0..(length(x)-1)], l3 <- [0..(length(x)-1)],
                                    let z3 = posarfitxajugador (posarfitxajugador (Tauler x) Ordinador i) Participant l2,
                                    let z4 = posarfitxajugador (posarfitxajugador z3 (Ordinador) (greedy z3)) Participant l3]


transpose :: [[Int]] -> [[Int]]
--En primer lloc, no es exactament la transposta d'una matriu.

--Funcio que donat el tauler, ho transforma en un altre tauler perque sigui facil d'escriure
--Exemple t1 -> [[1,1,0],[0,0,0],[1,0,0]]  ----> [[0,0,0],[1,0,0],[1,0,1]]. D'aquesta manera, quan
--fem print, escriurem les files de dalt del tauler, ...despres les del mig i finalment les 
--del principi  
transpose ([]:_) = []
transpose  x = (map last x) : transpose (map init x)

--caracters es una funcio que simplement canvia el tauler, per tal de fer-lo d'una
--forma mes agradable a la vista
caracters = \x -> 
    if (x == 1) then 'X'
    else
        if (x == 2) then 'O'
        else
            '-'

escriutauler :: [[Int]] -> IO()
--escriu un tauler, en linies diferents
escriutauler [] = do 
    putStrLn("")
escriutauler (xs:x) = do 
    print(map caracters xs)
    escriutauler x

extreutauler :: Tauler -> [[Int]]
--Donat un Tauler ens retorna la seva matriu
extreutauler (Tauler t1) = t1

jugar :: Tauler -> Int -> IO ()
--Jugar es la funcio que accepta un nivell, demana a quina columna vol el Participant
--posar la seva fitxa, crida a la resposta de l'ordinador i fa print del nou Tauler que queda
jugar (Tauler t1) level
    | level == 1 =
        do
            if ((evaluarTauler (Tauler t1) Participant) >= 4) then do
                --Mira si el Participant ha guanyat
                putStrLn("You won!")
                escriutauler (transpose t1)

            else
                if ((evaluarTauler (Tauler t1) Ordinador) >= 4) then do
                    --Mira si el l'Ordinador ha guanyat
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
                    --Veiem com l'Ordinador per decidir el seu nou moviment, computa un greedy
                    --sobre el tauler resultant
    | level == 3 =
        do
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
                    jugar (posarfitxajugador taulernou Ordinador (minmax taulernou)) level
                    --L'ordinador fa servir la funcio minmax per decidir on posar la nova fitxa

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
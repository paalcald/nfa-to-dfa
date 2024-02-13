module FA where
import qualified Data.List as L
import qualified Data.Map as M

-- Usaremos los siguientes aliases por sencillez.
type State = String
type States = [State]
type Symbol = Char
type Alphabet = [Symbol]
type DeltaN = State -> Symbol -> States
type DeltaD = State -> Symbol -> State

data Delta = DeltaN | DeltaD
-- Definiendo asi el ADT para implementar un automata finito
data FA = DFA States Alphabet DeltaD State States
        | NFA States Alphabet DeltaN State States

{-
alph = [ 'a', 'b']
qs = [ "q0", "q1","q2"]
deltaN "q0" 'a' = ["q0"]
deltaN "q0" 'b' = ["q1", "q2"]
deltaN "q1" 'a' = ["q1", "q2"]
deltaN "q1" 'b' = ["q2"]
deltaN "q2" 'a' = ["q0", "q1"]
deltaN "q2" 'b' = ["q1"]
myNFA = NFA qs alph deltaN "q0" ["q2"]
-}

--Dada una lista de simbolos y un automata finito comprueba que pertenecen
-- al alfabeto de este.
mayBeAccepted :: [Symbol] -> FA -> Bool
mayBeAccepted str fa = 
    and [c `elem` alph | c <- str] where
        alph = getAlphabet fa

--Dado una lista de simbolos del alfabeto de un automata finito, devuelve 
-- si la palabra que conforman esta aceptada o no por este.
isAccepted :: [Symbol] -> FA -> Bool
isAccepted myWord (DFA qs alpha delta q0 fs) = 
    (foldl delta q0 myWord) `elem` fs
isAccepted myWord (NFA qs alpha delta q0 fs) = 
    not (foldr1 (&&) [q `notElem` fs |q <- (foldl deltaD' [q0] myWord)])
    where
    deltaNToDeltaD' delta qs a = 
        L.sort . L.nub $ foldr1 (++) ( map ((flip delta) a) qs )  
    deltaD' q a = deltaNToDeltaD' delta q a

--Procedimientos para obtener los diferentes elementos de la clase. 
getAlphabet :: FA -> [Symbol]
getAlphabet (NFA _ alph _ _ _) = alph
getAlphabet (DFA _ alph _ _ _) = alph

getQs :: FA -> States
getQs (NFA qs _ _ _ _ ) = qs
getQs (DFA qs _ _ _ _ ) = qs

getFs :: FA -> States
getFs (NFA _ _ _ _ fs) = fs
getFs (DFA _ _ _ _ fs) = fs

getDeltaD :: FA -> DeltaD
getDeltaD (DFA _ _ delta _ _) = delta

getDeltaN :: FA -> DeltaN
getDeltaN (NFA _ _ delta _ _) = delta

getQ0 :: FA -> State
getQ0 (NFA _ _ _ q0 _) = q0
getQ0 (DFA _ _ _ q0 _) = q0

-- Conversor de NFA a DFA y viceversa, va creando nuevos estados
-- correspondientes a todas las posibilidades de NFA siendo cada uno
-- distinto hasta que no aparezcan nuevos, momento en el cual por medio
-- de un map da por concluido el calculo del nuevo delta y hace labores
-- de concatenacion, para renombrar los nuevos estados de manera que
-- el tipo de delta concuerde con el que ha de tener.
toDFA (DFA qs alph delta q0 fs ) = DFA qs alph delta q0 fs
toDFA (NFA qs alph delta q0 fs ) = DFA qs' alph delta' q0 fs' where
    delta' q a = concat (delta'' (qs'toqs'' M.! q) a)
    delta'' q a = deltaNToDeltaD' delta q a
    qs''= qsToQs' [[q0]]
    qs'toqs'' = M.fromList (zip qs' qs'')
    qs' = map (concat) qs''
    fs' = [concat q | f <- fs, q <- qs'', f `elem` q]
    qsToQs' qs
        | no_hay_nuevos = L.nub $ qs
        | otherwise = L.nub $ qsToQs' qs_n
        where 
        qs_n = [ delta'' q a | q <- qs, a <- alph]
        no_hay_nuevos = and [q `elem` qs | q <- qs_n]
    deltaNToDeltaD' delta qs a = 
        L.sort . L.nub $ concat ( map ((flip delta) a) qs )  
    deltaD' q a = deltaNToDeltaD' delta q a

toNFA (NFA qs alph delta q0 fs ) = NFA qs alph delta q0 fs
toNFA (DFA qs alph delta q0 fs ) = NFA qs alph delta' q0 fs
    where 
    delta' q a = [delta q a]
-- Posibles mejoras: El uso de una funcion para describir una relacion 
-- discreta entre claves me parece peligroso y da lugar a errores de 
-- dificil rastreamiento, esto se simplificaria utilizando el tipo de 
-- datos de modulo Data.Map de manera mas intensiva, pero posiblemente 
-- comprometiendo la consistencia del codigo he querido limitar la 
-- aplicacion de modulos externos.

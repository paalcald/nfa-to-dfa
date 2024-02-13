import System.IO
import Control.Monad
import qualified FA as FA
import qualified Data.Map as M

-- Me gustaria haber implementado algun tipo de interfaz interactiva
-- pero al final he optado por esta implementacion, una mejora o 
-- ampliacion que considero no seria demasiado dificil seria crear otro
-- archivo que en lugar de preguntas usase el entorno en linea de comando
-- de haskell para implementar un help, que explique las opciones 
-- disponibles y comandos como load 'fileName', write 'filename',
-- check 'string', convertToDFA 'inputfilename' 'outputfilename' etc etc

main = do
    putStr "Introduce el archivo del Automata Finito \n"
    fileName <- getLine
    myNFA <- getFAFromFile fileName
    palabra <- getValidWord myNFA
    let myDFA = FA.toDFA myNFA
        wasAccepted = FA.isAccepted palabra myNFA
        acceptancyString wasAccepted
            | wasAccepted = "La palabra " ++ palabra 
                ++ " pertenece al lenguaje del automata."
            | not wasAccepted = "La palabra " ++ palabra
                ++ " no pertenece al lenguaje descrito por el  automata."
            | otherwise = "si ves esto la he liado parda."
    putStrLn (acceptancyString wasAccepted)
    yesOrNo <- yesOrNoString "Quieres guardar el DFA a un archivo?"
    when (yesOrNo) (saveFa myDFA)

-- Dado un automata finito, pregunta donde quieres guardarlo e invoca
-- la accion encargada de guardarlo. 
saveFa :: FA.FA -> IO ()
saveFa fa = do
    putStrLn "Introduce el archivo para guardar el AF"
    writeFileName <- getLine
    putFAToFileName (FA.toDFA fa) writeFileName

-- Dado un string conteniendo una pregunta, se la hace al usuario
-- y devuelve la respuesta como booleano.
yesOrNoString :: String -> IO Bool
yesOrNoString inputString = do
    output <- getValidInputYesOrNo inputString 
    return output


-- Dado un automata finito pregunta al usuario por una palabra
-- hasta recibir una valida dentro de su alfabeto y entonces
-- devuelve un string representando si el lenguaje la acepta o
-- si en caso contrario no la acepta.
getValidWord :: FA.FA -> IO String
getValidWord fa = do
    putStrLn ("El alfabeto de entrada es: " ++ FA.getAlphabet fa)
    putStrLn ("Introduce una palabra valida.")
    inputString <- getLine
    if FA.mayBeAccepted inputString fa
        then return inputString
        else do putStrLn (inputString ++ " no es valida.")
                getValidWord fa

-- Dado un string con una pregunta se la hace al usuario hasta obtener
-- una respuesta valida [Y/n], devuelve el booleano correspondiente a la
-- eleccion del usuario, siendo yes True y no False.
getValidInputYesOrNo :: String -> IO Bool
getValidInputYesOrNo inputString = do
    putStr (inputString ++ " [Y/n]\n")
    userInput <- getLine
    case (isValidYesOrNo userInput) of
        Just True -> return True
        Just False -> return False
        Nothing -> getValidInputYesOrNo ""

-- Dado un string, devuelve si este tiene el formato valido de un si,
-- el formato valido de un no, o en caso contrario nada.
isValidYesOrNo :: String -> Maybe Bool
isValidYesOrNo userInput
    | userInput `elem` validYes = Just True
    | userInput `elem` validNo = Just False
    | otherwise = Nothing
    where 
    validYes = [ "y", "Y", "yes", "Yes", "YES" ]
    validNo = ["n", "N", "no", "No", "NO"]

---------------------------------------------------------------------------
-- El Formato escogido para guardar y leer un FA es el siguiente         --
---------------------------------------------------------------------------
-- q0 q1 q2 ...     // Estados posibles, q0 el primero                   --
-- a b ...          // Alfabeto de entrada.                              --
-- f0 f1 ...        // Estados de aceptacion.                            --
-- delta q0 a       // Valor del mapeo por delta, ordenado por estados   --
-- delta q0 b       // y posteriormente por letra del alfabeto           --
-- delta q1 a       // sea un unico elemento o una varios siendo estos   --
-- ...              // separados por espacios en el caso lista.          --
---------------------------------------------------------------------------             
-- Las siguientes acciones gestionan la lectura y escritura de un automata
-- finito en un archivo.

putFAToFileName :: FA.FA -> String -> IO ()
putFAToFileName (FA.NFA qs alph delta q0 fs) fileName = do
    myFAFile <- openFile fileName WriteMode
    hPutStrLn myFAFile ( unwords qs )
    hPutStrLn myFAFile ( unwords  [ [s] | s <- alph ] )
    hPutStrLn myFAFile ( unwords fs )
    let delta_lista = [ delta q a | q <- qs, a <- alph]
    hPutStrLn myFAFile ( unlines [ unwords d | d <- delta_lista] )
    hClose myFAFile

putFAToFileName (FA.DFA qs alph delta q0 fs) filename = do
    myFAFile <- openFile filename WriteMode
    hPutStrLn myFAFile ( unwords qs )
    hPutStrLn myFAFile ( unwords [ [s] | s <- alph] )
    hPutStrLn myFAFile ( unwords fs )
    let delta_lista = [ delta q a | q <- qs, a <- alph]
    hPutStr myFAFile ( unlines delta_lista )
    hClose myFAFile

getFAFromFile :: String -> IO FA.FA
getFAFromFile fileName = do
    myFAFile <- openFile fileName ReadMode
    q_line <- hGetLine myFAFile
    let qs = (words q_line)
        q0 = qs !! 0 
    alph_line <- hGetLine myFAFile
    let alph_ = (words alph_line)
        alph = (foldr1 (++) alph_)
    f_line <- hGetLine myFAFile
    let fs = (words f_line)
    lista_line <- hGetContents myFAFile
    let delta_line = map words (lines lista_line)
        qq = foldr1 (++) [take (length alph) (repeat q) | q <- qs]
        deltaTriplete = zip3 qq (cycle alph) delta_line     
        deltaMap =  M.fromList [ ((q,a), r) | (q,a,r) <- deltaTriplete]
        delta q a = deltaMap M.! (q,a) 
        salida = [delta q a | q <- qs, a <- alph]
        my2NFA = FA.NFA qs alph delta q0 fs
        myDFA = FA.toDFA my2NFA
    return myDFA


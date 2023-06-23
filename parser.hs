module Parser
where
import Text.Parsec.String
import System.Environment
import DSLFunc
import DSLData
import Text.Read (readMaybe)
import Data.Maybe

{-parsea la condicion-}
parseCondition::String->Maybe(Int->Bool)
parseCondition s = case s of
    '<':rest -> readValue rest >>= \x -> Just(lessThan x)
    '>':rest -> readValue rest >>= \x -> Just(greatherThan x)
    '=':'=' : rest->readValue rest >>= \x -> Just(equalTo x)
    _ -> Nothing
    where
        readValue = readMaybe::String->Maybe Int
        lessThan x = (< x)
        greatherThan x = (> x)
        equalTo x = (== x)

{-parsea la funcion de ordenacion-}
parseSortingFunction::String->Maybe(String->[String])
parseSortingFunction s = case s of
    "sortedHidrophob"->Just sortedHidrophob
    _ -> Nothing

{-parsea la tupla-}
parseTuple::String->Maybe(String, Int)
parseTuple line = case words (filter (/=',')line) of
    [id, val] -> do
        intVal <- readMaybe val
        return (id, intVal)
    _ -> Nothing

{-parsea la funcion de similitud-}
parseFunction::String->Maybe(String->String->Int)
parseFunction s = case s of
    "occurrences" -> Just occurrences
    "charsDistance" -> Just charsDistance
    "levenshteinM" -> Just levenshteinM
    "levenshtein" -> Just levenshtein
    "lcs" -> Just lcs
    _ ->Nothing

{-parsea la entrada-}
parseInput :: String -> Maybe Request
parseInput input = case words input of
    ["Select", bullseye, "from", file, "apply" ,function]->Just $ SelectRequest bullseye file function
    ["Open", file, "where", condition] -> Just $ OpenRequest file condition
    ["Open", file,"sortBy", sortingFunction ]->Just $ SortRequest file sortingFunction
    ("Put":xs) -> Just $ PutRequest (init xs) (last xs)
    _ -> Nothing



--FUNCIONES QUE DEVUELVEN LA CONSOLA
{-Retorna el valor que se imprime cuando se elige la funcion select-}
selectValue::String->String->String->String
selectValue bs file function = 
    result
    where
        func = parseFunction function
        sf = simFunc (fromJust func) file bs
        tuples = getTuples file bs sf
        strTup = map show tuples        
        result = (unlines strTup)

{-Retorna el valor que se imprime cuando se elige la funcion open-}
openValue::String->String->String
openValue file condition = 
    result
    where
        cond = parseCondition condition
        filteredProteins = filterProteins file (fromMaybe (const True) cond)
        result = unlines filteredProteins

{-Retorna el valor que se imprime cuando se elige la funcion sort-}
sortValue::String->String->String
sortValue file sortingFunction = 
    result
    where
        sortFunc = parseSortingFunction sortingFunction
        sFunc = (fromJust sortFunc) file
        result = (unlines sFunc)
            

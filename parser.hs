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
    ["Select", bullseye, "from", file, "apply" ,function, "saveIn", fileToSave]->Just $ SelectRequest bullseye file function fileToSave
    ["Open", file, "where", condition] -> Just $ OpenRequest file condition
    ["Open", file, "where", condition, "saveIn", fileToSave] ->Just $ OpenAndSaveRequest file condition fileToSave
    ["Open", file,"sortBy", sortingFunction ]->Just $ SortRequest file sortingFunction
    ["Put(" ,request, ")in", file] -> Just $ PutRequest request file
    _ -> Nothing
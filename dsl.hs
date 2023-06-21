module DSL
where
import Text.Parsec
import Text.Parsec.String
import System.Environment
import DSLFunc
import DSLData
import Text.Read (readMaybe)
import Data.Maybe

main::IO()
main = do
    consoleInput <- getLine
    let maybeInput = parseInput consoleInput
    case maybeInput of 
        Nothing -> putStrLn "Invalid input"
        Just (SelectRequest {bullseye=bullseye, file=file}) -> do
            function <- getUserFunction
            putStrLn $ "Selected function: "++function
            let funcType = (undefined :: String->String->Int)
            let func = read function :: String->String->Int
            nameOfFile <- getResultName
            putStrLn $ "Selected file to save the results: "++nameOfFile
            let sf = simFunc func file bullseye
            getTuples file bullseye sf nameOfFile
            putStrLn $ "Successfully completed"
        Just (OpenRequest {file=file, condition=condition}) -> do
            let cond = parseCondition condition
            let filteredProteins = filterProteins file (fromMaybe (const True) cond)
            putStrLn (unlines filteredProteins)        

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


parseTuple::String->Maybe(String, Int)
parseTuple line = case words (filter (/=',')line) of
    [id, val] -> do
        intVal <- readMaybe val
        return (id, intVal)
    _ -> Nothing

instance Read (String -> String -> Int) where
  readsPrec _ str = [(func, "")]
    where func s1 s2 = length s1 + length s2

getUserFunction :: IO String
getUserFunction = do
  putStrLn "Which function do you want to use?"
  input <- getLine
  if null input
    then do
        putStrLn "Please enter a function name."
        getUserFunction
    else
        return input


getResultName::IO String
getResultName = do
    putStrLn "Where do you want to save the results?"
    getLine

parseInput :: String -> Maybe Request
parseInput input = case words input of
    ["Select", bullseye, "from", file]->Just $ SelectRequest bullseye file
    ["Open", file, "where", condition] -> Just $ OpenRequest file condition
    _ -> Nothing
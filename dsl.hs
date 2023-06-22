module DSL
where
import Text.Parsec
import Text.Parsec.String
import System.Environment
import DSLFunc
import DSLData
import Text.Read (readMaybe)
import Data.Maybe
import Parser

main::IO()
main = do
    consoleInput <- getLine
    let maybeInput = parseInput consoleInput
    case maybeInput of 
        Nothing -> putStrLn "Invalid input"
        Just (SelectRequest {bullseye=bullseye, file=file, function = function, fileToSave = fileToSave}) -> do
            let func = parseFunction function
            let sf = simFunc (fromJust func) file bullseye
            getTuples file bullseye sf fileToSave
            putStrLn $ "Successfully completed"
        Just (OpenRequest {file=file, condition=condition}) -> do
            let cond = parseCondition condition
            let filteredProteins = filterProteins file (fromMaybe (const True) cond)
            putStrLn (unlines filteredProteins)
            putStrLn $ "Successfully completed"        
        Just (SortRequest {file = file, sortingFunction = sortingFunction}) -> do
            let sortFunc = parseSortingFunction sortingFunction
            let sFunc = (fromJust sortFunc) file
            putStrLn (unlines sFunc)
            putStrLn $ "SuccessfullyCompleted"
        Just (OpenAndSaveRequest{file=file, condition = condition, fileToSave = fileToSave})->do
            let cond = parseCondition condition
            let filteredProteins = filterProteins file (fromMaybe (const True) cond)
            writeFile fileToSave (unlines filteredProteins)
            putStrLn $ "Successfully completed"
        

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
        Just (SelectRequest {bullseye=bullseye, file=file, function = function}) -> do
            let strTup = selectValue bullseye file  function
            putStrLn strTup
            putStrLn $ "Successfully completed"
        Just (OpenRequest {file=file, condition=condition}) -> do
            let filteredProteins = openValue file condition
            putStrLn filteredProteins
            putStrLn $ "Successfully completed"        
        Just (SortRequest {file = file, sortingFunction = sortingFunction}) -> do
            let sValues = sortValue file sortingFunction
            putStrLn sValues
            putStrLn $ "SuccessfullyCompleted"
        Just (PutRequest{request=request, fileToSave = fileToSave})->do
            let tailLessRequest = init request
            let stringRequest = unwords tailLessRequest
            let maybeRequest = parseInput stringRequest
            case maybeRequest of 
                Nothing-> putStrLn "Invalid request"
                Just (SelectRequest{bullseye= bullseye, file= file, function = function})-> do
                    let strTuples = selectValue bullseye file function
                    writeFile fileToSave strTuples
                    putStrLn $ "Successfully completed"
                Just (OpenRequest{file=file, condition = condition})->do
                    let filteredProteins = openValue file condition
                    writeFile fileToSave filteredProteins
                    putStrLn $ "Successfully completed"
                Just (SortRequest {file = file, sortingFunction})->do
                    let sValues = sortValue file sortingFunction
                    writeFile fileToSave sValues
                    putStrLn $ "Successfully completed"


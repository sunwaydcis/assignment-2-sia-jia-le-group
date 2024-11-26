module Main where

import Data.List (maximumBy, groupBy, sortOn, elemIndex)
import Data.Ord (comparing)
import Data.Ratio
import Data.Function (on)
import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Char (toLower)

-- https://blog.ploeh.dk/2024/02/19/extracting-data-from-a-small-csv-file-with-haskell/
-- Learn from this source to extract CSV file.
csvData :: IO [String]
csvData = lines <$> readFile "hospital.csv"

splitData :: [String] -> [[String]]
splitData = map (splitOn ",")

removeHeader :: [[String]] -> [[String]]
removeHeader = tail 

result :: IO [[String]]
result = do
    dataset <- csvData
    let dataWithoutHeader = removeHeader (splitData dataset)
    return dataWithoutHeader

headerValue :: IO [String]
headerValue = do
    csvData >>= \dataset -> return (head (splitData dataset))

extractColumnbyFieldName :: String -> IO [String]
extractColumnbyFieldName field = 
    headerValue >>= \header ->
    result >>= \datalist -> 
    let headerlowercase = map (map toLower) header
        fieldlowercase = map toLower field
        fieldIndex = fromJust (elemIndex fieldlowercase headerlowercase) 
    in return $ map (!! fieldIndex) datalist

stringdatalist :: String -> IO [String]
stringdatalist fieldname =
    extractColumnbyFieldName fieldname >>= \list -> return $ list

integerdatalist :: String -> IO [Integer]
integerdatalist fieldname = 
    extractColumnbyFieldName fieldname >>= \list -> return $ map read list
-- --------------------------------------------------------------------------------------------------------------------------------

-- -------------------------------------- Code for question 1 ---------------------------------------------------------------------
-- -- Create tuple 2 for each states and their number of beds
extractTuple :: IO [(String, Integer)]
extractTuple = zip <$> stringdatalist "state" <*> integerdatalist "beds"

largestByBeds :: IO (String, Integer)
largestByBeds = 
    extractTuple >>= \tupleList -> 
    return $ maximumBy (comparing snd) tupleList

firstValueofTuple :: IO String
firstValueofTuple = fst <$> largestByBeds

question1 :: IO ()
question1 = do
    stateValue <- firstValueofTuple
    putStrLn $ "State with the largest number of beds is: " ++ show stateValue 

-- --------------------------------------------------------------------------------------------------------------------------------

-- ------------------------------------------------------ Question 2 --------------------------------------------------------------
-- Get total bed based on category for a HospitalData record
tupleForBeds :: IO [(String, Integer)]
tupleForBeds = zip <$> stringdatalist "state" <*> integerdatalist "beds"

tupleForBedsCovid :: IO [(String, Integer)]
tupleForBedsCovid = zip <$> stringdatalist "state" <*> integerdatalist "beds_covid"

bedsRatio :: IO String
bedsRatio =
    integerdatalist "beds" >>= \totalBeds ->
    integerdatalist "beds_covid" >>= \totalBedsCovid ->
    let ratio = fromIntegral (sum totalBedsCovid) % fromIntegral (sum totalBeds)
        ratioDivision = fromIntegral (sum totalBedsCovid) / fromIntegral (sum totalBeds)
    in return $ show (numerator ratio) ++ ":" ++ show (denominator ratio) ++ " Ratio in decimal: " ++ show ratioDivision

question2 :: IO ()
question2 = 
    bedsRatio >>= \ratio -> 
    putStrLn $ "The ratio is: " ++ ratio
-- --------------------------------------------------------------------------------------------------------------------


-- ------------------------------------------------------ Question 3 ---------------------------------------------------
tupleForSuspected :: IO [(String, Integer)]
tupleForSuspected = zip <$> stringdatalist "state" <*> integerdatalist "admitted_pui"

tupleForCovidPositive :: IO [(String, Integer)]
tupleForCovidPositive = zip <$> stringdatalist "state" <*> integerdatalist "admitted_covid"

average :: [Integer] -> Double
average value = fromIntegral (sum value) / fromIntegral (length value)

averageByStateForSuspected :: IO [(String, Double)]
averageByStateForSuspected =
    tupleForSuspected >>= \suspectedTupleList -> 
    let suspectedTupleByState = groupBy ((==) `on` fst) $ sortOn fst suspectedTupleList
    in return $ map (\group -> (fst (head group), average (map snd group))) suspectedTupleByState

averageByStateForCovidPositive :: IO [(String, Double)]
averageByStateForCovidPositive = 
    tupleForCovidPositive >>= \covidPositiveTupleList -> 
    let covidPositiveTupleByState = groupBy ((==) `on` fst) $ sortOn fst covidPositiveTupleList
    in return $ map (\group -> (fst (head group), average (map snd group))) covidPositiveTupleByState

question3 :: IO ()
question3 = do
    suspectedAverages <- averageByStateForSuspected
    covidPositiveAverages <- averageByStateForCovidPositive
    putStrLn $ "The Average of Individuals in Category Suspected That Is Being Admitted to Hospitals for Each State: "
    mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) suspectedAverages
    putStrLn $ ""
    putStrLn $ "The Average of Individuals in Category Covid-19 Positive That Is Being Admitted to Hospitals for Each State: "
    mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) covidPositiveAverages

main :: IO ()
main = do
    question1
    putStrLn $ ""
    question2
    putStrLn $ ""
    question3
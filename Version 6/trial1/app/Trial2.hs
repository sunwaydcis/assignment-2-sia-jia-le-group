module Main where

import Data.List (maximumBy, groupBy, sortOn, elemIndex)
import Data.Ord (comparing)
import Data.Ratio
import Data.Function (on)
import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Char (toLower)

csvData :: IO [String]
csvData = lines <$> readFile "hospital.csv"

splitData :: [String] -> [[String]]
splitData = map (splitOn ",")

removeHeader :: [[String]] -> [[String]]
removeHeader = tail 

result :: IO [[String]]
result = do
    datalist <- csvData
    let dataWithoutHeader = removeHeader (splitData datalist)
    return dataWithoutHeader

headerValue :: IO [String]
headerValue = do
    headerlist <- csvData
    return (head (splitData headerlist))

extractColumnbyField :: String -> IO [String]
extractColumnbyField field = do
    header <- headerValue
    datalist <- result
    let headerlowercase = map (map toLower) header
    let fieldlowercase = map toLower field
    let fieldIndex = fromJust (elemIndex fieldlowercase headerlowercase)
    return $ map (!! fieldIndex) datalist

date :: IO [String]
date = extractColumnbyField "date"

state :: IO [String]
state = extractColumnbyField "state"

beds :: IO [Integer]
beds = do 
    bedsList <- extractColumnbyField "beds"
    return $ map read bedsList

beds_covid :: IO [Integer]
beds_covid = do 
    beds_covidlist <- extractColumnbyField "beds_covid"
    return $ map read beds_covidlist

beds_noncrit :: IO [Integer]
beds_noncrit = do
    beds_noncritlist <- extractColumnbyField "beds_noncrit"
    return $ map read beds_noncritlist

admitted_pui :: IO [Integer]
admitted_pui = do 
    admitted_puilist <- extractColumnbyField "admitted_pui"
    return $ map read admitted_puilist

admitted_covid :: IO [Integer]
admitted_covid = do
    admitted_covidlist <- extractColumnbyField "admitted_covid"
    return $ map read admitted_covidlist

admitted_total :: IO [Integer]
admitted_total = do
    admitted_totallist <- extractColumnbyField "admitted_total"
    return $ map read admitted_totallist

discharged_pui :: IO [Integer]
discharged_pui = do
    discharged_puilist <- extractColumnbyField "discharged_pui"
    return $ map read discharged_puilist

discharged_covid :: IO [Integer]
discharged_covid = do
    discharged_covidlist <- extractColumnbyField "discharged_covid"
    return $ map read discharged_covidlist

discharged_total :: IO [Integer]
discharged_total = do
    discharged_totallist <- extractColumnbyField "discharged_total"
    return $ map read discharged_totallist

hosp_covid :: IO [Integer]
hosp_covid = do
    hosp_covidlist <- extractColumnbyField "hosp_covid"
    return $ map read hosp_covidlist

hosp_pui :: IO [Integer]
hosp_pui = do 
    hosp_puilist <- extractColumnbyField "hosp_pui"
    return $ map read hosp_puilist

hosp_noncovid :: IO [Integer]
hosp_noncovid = do 
    hosp_noncovidlist <- extractColumnbyField "hosp_noncovid"
    return $ map read hosp_noncovidlist

-- --------------------------------------------------------------------------------------------------------------------------------

-- -------------------------------------- Code for question 1 ---------------------------------------------------------------------
-- -- Create tuple 2 for each states and their number of beds
extractTuple :: IO [(String, Integer)]
extractTuple = do
    stateList <- state
    bedslist <- beds
    return $ zip stateList bedslist

largestByBeds :: IO (String, Integer)
largestByBeds = do
    tupleList <- extractTuple
    return $ maximumBy (comparing snd) tupleList

firstValueofTuple :: IO String
firstValueofTuple = do
    stateWithLargestBeds <- largestByBeds
    return $ fst stateWithLargestBeds

secondValueofTuple :: IO Integer
secondValueofTuple = do
    numberOfBeds <- largestByBeds
    return $ snd numberOfBeds

question1 :: IO ()
question1 = do
    state <- firstValueofTuple
    bedsnum <- secondValueofTuple
    putStrLn $ "State with the largest number of beds is: " ++ show state

-- --------------------------------------------------------------------------------------------------------------------------------

-- ------------------------------------------------------ Question 2 --------------------------------------------------------------
-- Get total bed based on category for a HospitalData record
bedsRatio :: IO String
bedsRatio = do
    totalBeds <- beds
    totalBedsCovid <- beds_covid
    let num = numerator (fromIntegral (sum totalBedsCovid) % fromIntegral (sum totalBeds))
    let den = denominator (fromIntegral (sum totalBedsCovid) % fromIntegral (sum totalBeds))
    return $ show num ++ ":" ++ show den

question2 :: IO ()
question2 = do
    ratio <- bedsRatio
    putStrLn $ "The ratio is: " ++ ratio

-- --------------------------------------------------------------------------------------------------------------------


-- ------------------------------------------------------ Question 3 ---------------------------------------------------
tupleForSuspected :: IO [(String, Integer)]
tupleForSuspected = do
    stateList <- state
    suspectedList <- admitted_pui
    return $ zip stateList suspectedList

tupleForCovidPositive :: IO [(String, Integer)]
tupleForCovidPositive = do
    stateList <- state
    covidPositiveList <- admitted_covid
    return $ zip stateList covidPositiveList

average :: [Integer] -> Double
average value = fromIntegral (sum value) / fromIntegral (length value)

averageByStateForSuspected :: IO [(String, Double)]
averageByStateForSuspected = do
    suspectedTupleList <- tupleForSuspected
    let suspectedTupleByState = groupBy ((==) `on` fst) $ sortOn fst suspectedTupleList
    return $ map (\group -> (fst (head group), average (map snd group))) suspectedTupleByState

averageByStateForCovidPositive :: IO [(String, Double)]
averageByStateForCovidPositive = do
    covidPositiveTupleList <- tupleForCovidPositive
    let covidPositiveTupleByState = groupBy ((==) `on` fst) $ sortOn fst covidPositiveTupleList
    return $ map (\group -> (fst (head group), average (map snd group))) covidPositiveTupleByState

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
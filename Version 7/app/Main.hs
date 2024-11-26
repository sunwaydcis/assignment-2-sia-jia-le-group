-- {-# LANGUAGE OverloadedStrings #-} --Convert string literals to ByteString because cassava uses ByteString internally
-- module Main where

-- -- Data.Bytestring.Lazy explanation (need to cabal install --lib bytestring in order to use it).
-- -- Use to read data as a sequence of binary data. Data.Bytestring.Lazy enables large data
-- -- to be processed efficiently by not loading all the large file data into memory directly.
-- -- Instead, it load the data into the memory only when needed to reduce stress on memory.
-- -- The import qualified means the imported modules functions, types and other definitions must be assessed with a prefix (BL in our code).
-- -- We cannot the module's functions and types directly by their name. 
-- import qualified Data.ByteString.Lazy as BL 
-- import Data.Csv --this is from the cassava library
-- import Data.List (maximumBy, groupBy, sortOn)
-- import Data.Ord (comparing)
-- import Data.Ratio
-- import Data.Function (on)
-- import Text.Printf (printf)


-- -- Define the Date and Category types as before
-- type Date = String

-- data HospitalDataset = HospitalDataset
--     {date :: String
--      ,state :: String
--      ,beds :: Integer
--      ,beds_covid :: Integer
--      ,beds_noncrit :: Integer
--      ,admitted_pui :: Integer
--      ,admitted_covid :: Integer
--      ,admitted_total :: Integer
--      ,discharged_pui :: Integer
--      ,discharged_covid :: Integer
--      ,discharged_total :: Integer
--      ,hosp_covid :: Integer
--      ,hosp_pui :: Integer
--      ,hosp_noncovid :: Integer
--     } deriving (Show, Eq)

-- --Define how to get a hospital dataset from a record (CSV row) by implementing the FromNameRecord type class
-- instance FromNamedRecord HospitalDataset where
--     parseNamedRecord record =
--         HospitalDataset
--             <$> record .: "date"
--             <*> record .: "state"
--             <*> record .: "beds"
--             <*> record .: "beds_covid"
--             <*> record .: "beds_noncrit"
--             <*> record .: "admitted_pui"
--             <*> record .: "admitted_covid"
--             <*> record .: "admitted_total"
--             <*> record .: "discharged_pui"
--             <*> record .: "discharged_covid"
--             <*> record .: "discharged_total"
--             <*> record .: "hosp_covid"
--             <*> record .: "hosp_pui"
--             <*> record .: "hosp_noncovid"

-- --Function to read hospital data from a csv file
-- readHospitalData :: FilePath -> IO (Either String ([HospitalDataset]))
-- readHospitalData filePath = do 
--     csvData <- BL.readFile filePath
--     let result = decodeByName csvData
--     case result of
--         Left err -> return $ Left err
--         Right (_, records) -> return $ Right (foldr (:) [] records)


-- -- -- Code for question 1
-- -- Create tuple 2 for each states and their number of beds
-- extractTuple :: HospitalDataset -> (String, Integer)
-- extractTuple dataset = (state dataset, beds dataset)

-- processData :: [HospitalDataset] -> [(String, Integer)]
-- processData hospitalData = map extractTuple hospitalData

-- largestByBeds :: [(String, Integer)] -> (String, Integer)
-- largestByBeds = maximumBy (comparing snd)

-- firstValueofTuple :: (String, Integer) -> String
-- firstValueofTuple = fst 

-- question1 :: IO ()
-- question1 = do
--     result <- readHospitalData "hospital.csv"
--     case result of 
--         Left err -> putStrLn $ "Error Parsing CSV: " ++ err
--         Right hospitalData -> do
--             let tuples = processData hospitalData
--             let largestTuple = largestByBeds tuples
--             let state = firstValueofTuple largestTuple
--             putStrLn $ "State with the largest number of beds is: " ++ show state

-- -- ------------------------------------------------------Version 1-----------------------------------------
-- -- Question 2
-- -- Get total bed based on category for a HospitalData record
-- data Bed = Beds | BedsCovid | BedsNonCrit | All
--     deriving (Eq, Show)

-- bedByCategory :: Bed -> HospitalDataset -> Integer
-- bedByCategory Beds record = beds record
-- bedByCategory BedsCovid record = beds_covid record
-- bedByCategory BedsNonCrit record = beds_noncrit record
-- bedByCategory All record = beds record + beds_covid record + beds_noncrit record

-- -- Sum admitted or discharged by category for all records
-- totalbedByCategory :: Bed -> [HospitalDataset] -> Integer
-- totalbedByCategory bed = sum . map (bedByCategory bed)

-- question2 :: IO ()
-- question2 = do
--     result <- readHospitalData "hospital.csv"
--     case result of
--         Left err -> putStrLn $ "Error Parsing CSV: " ++ err
--         Right hospitalData -> do
--             let totalBedsCovid = totalbedByCategory BedsCovid hospitalData
--             let totalBeds = totalbedByCategory Beds hospitalData
--             let ratio = fromIntegral totalBedsCovid % fromIntegral totalBeds
--             let num = numerator (ratio)
--             let den = denominator (ratio)
--             putStrLn $ "The ratio is: " ++ show num ++ ":" ++ show den

-- -- Question 3
-- data Category = Suspected | CovidPositive
--     deriving Show

-- individualByCategory :: Category -> HospitalDataset -> Integer
-- individualByCategory Suspected record = admitted_pui record
-- individualByCategory CovidPositive record = admitted_covid record

-- createTupleByState :: [HospitalDataset] -> [(String, [HospitalDataset])]
-- createTupleByState list = 
--     map (\group -> (state (head group), group)) $
--         groupBy ((==) `on` state) $ sortOn state list

-- sumSuspected :: [HospitalDataset] -> Integer
-- sumSuspected sums = sum $ map (individualByCategory Suspected) sums

-- computeSuspectedAverage :: [(String, [HospitalDataset])] -> [(String, Double)]
-- computeSuspectedAverage suspectedData =  
--     map computeAverage suspectedData
--   where  
--     computeAverage (st, records) =
--         let count = fromIntegral (length records) :: Double
--             totalSuspected = fromIntegral (sumSuspected records) :: Double
--             average = totalSuspected / count
--         in (st, average)

-- sumCovid :: [HospitalDataset] -> Integer
-- sumCovid sums = sum $ map (individualByCategory CovidPositive) sums

-- computeCovidAverage :: [(String, [HospitalDataset])] -> [(String, Double)]
-- computeCovidAverage covidData =  
--     map computeAverage covidData
--   where  
--     computeAverage (st, records) =
--         let count = fromIntegral (length records) :: Double
--             totalCovid = fromIntegral (sumCovid records) :: Double
--             average = totalCovid / count
--         in (st, average)

-- question3 :: IO ()
-- question3  = do
--     result <- readHospitalData "hospital.csv"
--     case result of
--         Left err -> putStrLn $ "Error Parsing CSV: " ++ err
--         Right hospitalData -> do
--             let list = createTupleByState (hospitalData)
--             let suspectedavg = computeSuspectedAverage list
--             let covidavg = computeCovidAverage list
--             putStrLn $ "Average of Suspected individual in each states: "
--             mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) suspectedavg
--             putStrLn $ ""
--             putStrLn $ "Average of Individual in Category Covid-19 Positive for each states: "
--             mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) covidavg

-- main :: IO ()
-- main = do
--     question1
--     putStrLn $ ""
--     question2
--     putStrLn $ ""
--     question3


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

question1 :: IO ()
question1 = do
    stateValue <- firstValueofTuple
    putStrLn $ "State with the largest number of beds is: " ++ show stateValue 

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
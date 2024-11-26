{-# LANGUAGE OverloadedStrings #-} --Convert string literals to ByteString because cassava uses ByteString internally
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv --this is from the cassava library
import qualified Data.Vector as V
import Data.List (maximumBy, groupBy, sortOn)
import Data.Ord (comparing)
import Data.Ratio
import Data.Function (on)
import Text.Printf (printf)


-- Define the Date and Category types as before
type Date = String

data HospitalDataset = HospitalDataset
    {date :: String
     ,state :: String
     ,beds :: Integer
     ,beds_covid :: Integer
     ,beds_noncrit :: Integer
     ,admitted_pui :: Integer
     ,admitted_covid :: Integer
     ,admitted_total :: Integer
     ,discharged_pui :: Integer
     ,discharged_covid :: Integer
     ,discharged_total :: Integer
     ,hosp_covid :: Integer
     ,hosp_pui :: Integer
     ,hosp_noncovid :: Integer
    } deriving (Show, Eq)

--Define how to get a hospital dataset from a record (CSV row) by implementing the FromNameRecord type class
instance FromNamedRecord HospitalDataset where
    parseNamedRecord record =
        HospitalDataset
            <$> record .: "date"
            <*> record .: "state"
            <*> record .: "beds"
            <*> record .: "beds_covid"
            <*> record .: "beds_noncrit"
            <*> record .: "admitted_pui"
            <*> record .: "admitted_covid"
            <*> record .: "admitted_total"
            <*> record .: "discharged_pui"
            <*> record .: "discharged_covid"
            <*> record .: "discharged_total"
            <*> record .: "hosp_covid"
            <*> record .: "hosp_pui"
            <*> record .: "hosp_noncovid"

--Function to read hospital data from a csv file
readHospitalData :: FilePath -> IO (Either String (V.Vector HospitalDataset))
readHospitalData filePath = do 
    csvData <- BL.readFile filePath
    return $ fmap snd $ decodeByName csvData --Decode by name uses the header row for named fields


-- Code for question 1
instance FromNamedRecord (String, Integer) where
    parseNamedRecord bedrecord =
        (,) <$> bedrecord .: "state" <*> bedrecord .: "beds"

-- Create tuple 2 for each states and their number of beds
extractTuple :: HospitalDataset -> (String, Integer)
extractTuple dataset = (state dataset, beds dataset)

processData :: V.Vector HospitalDataset -> [(String, Integer)]
processData hospitalData = V.toList $ V.map extractTuple hospitalData

largestByBeds :: [(String, Integer)] -> (String, Integer)
largestByBeds = maximumBy (comparing snd)

firstValueofTuple :: (String, Integer) -> String
firstValueofTuple = fst 

question1 :: IO ()
question1 = do
    result <- readHospitalData "hospital.csv"
    case result of 
        Left err -> putStrLn $ "Error Parsing CSV: " ++ err
        Right hospitalData -> do
            -- let totalCovidAdmissions = totalAdmittedByCategory CovidPositive hospitalData
            -- putStrLn $ "Total COVID-19 Admissions: " ++ show totalCovidAdmissions
            let tuples = processData hospitalData
            let largestTuple = largestByBeds tuples
            let state = firstValueofTuple largestTuple
            putStrLn $ "State with the largest number of beds is: " ++ show state


-- Question 2
-- Get total bed based on category for a HospitalData record
data Bed = Beds | BedsCovid | BedsNonCrit | All
    deriving (Eq, Show)

bedadmittedByCategory :: Bed -> HospitalDataset -> Integer
bedadmittedByCategory Beds record = beds record
bedadmittedByCategory BedsCovid record = beds_covid record
bedadmittedByCategory BedsNonCrit record = beds_noncrit record
bedadmittedByCategory All record = beds record + beds_covid record + beds_noncrit record

-- Sum admitted or discharged by category for all records
totalbedByCategory :: Bed -> V.Vector HospitalDataset -> Integer
totalbedByCategory bed = V.sum . V.map (bedadmittedByCategory bed)

question2 :: IO ()
question2 = do
    result <- readHospitalData "hospital.csv"
    case result of
        Left err -> putStrLn $ "Error Parsing CSV: " ++ err
        Right hospitalData -> do
            let totalBedsCovid = totalbedByCategory BedsCovid hospitalData
            let totalBeds = totalbedByCategory All hospitalData
            let ratio = fromIntegral totalBedsCovid % fromIntegral totalBeds
            let num = numerator (ratio)
            let den = denominator (ratio)
            putStrLn $ "The ratio is: " ++ show num ++ ":" ++ show den

-- Question 3
data Category = Suspected | CovidPositive deriving Show

-- Extract count based on category
individualByCategory :: Category -> HospitalDataset -> Integer
individualByCategory Suspected record = admitted_pui record
individualByCategory CovidPositive record = admitted_covid record

-- Group records by state
createList :: Category -> V.Vector HospitalDataset -> [(String, [Integer])]
createList category hospitalData =
    let tuples = V.toList $ V.map (\record -> (state record, individualByCategory category record)) hospitalData
    in map (\group -> (fst (head group), map snd group)) $
       groupBy ((==) `on` fst) $ sortOn fst tuples

-- Compute totals and averages
sumCategory :: [Integer] -> Integer
sumCategory = sum

countRecords :: [Integer] -> Int
countRecords = length

-- Main computation
question3 :: IO ()
question3 = do
    result <- readHospitalData "hospital.csv"
    case result of
        Left err -> putStrLn $ "Error Parsing CSV: " ++ err
        Right hospitalData -> do
            let suspectedgroupedList = createList Suspected hospitalData
            let suspectedaverages = map (\(st, counts) ->
                                 (st, fromIntegral (sumCategory counts) / fromIntegral (countRecords counts) :: Double)) suspectedgroupedList

            let covidgroupedList = createList CovidPositive hospitalData
            let covidaverages = map (\(st, counts) ->
                                 (st, fromIntegral (sumCategory counts) / fromIntegral (countRecords counts) :: Double)) covidgroupedList
            putStrLn $ ""
            putStrLn $ "Averages of Individuals in Category Suspected Admitted to Hospital for each State: "
            mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) suspectedaverages
            putStrLn $ ""
            putStrLn $ "Averages of Individuals in Category Covid-19 Positive Admitted to Hospital for each State: "
            mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) covidaverages

main :: IO ()
main = do
    question1
    question2
    question3












-- -------------------------------------------------Wrong Answer---------------------------------------------------------
-- import Prelude hiding (filter)
-- import System.Directory (doesFileExist)
-- datatype to model a hospital dataset
            -- type ErrorMsg = String
-- --type sysnonym to handle CSV contents
-- type CSVData = (Header, V.Vector HospitalDataset)

-- filePath :: String
-- filePath = "hospital.csv"
-- --Funtion to read the CSV
-- parseCSV :: FilePath -> IO (Either ErrorMsg CSVData)
-- parseCSV filePath = do
--     fileExists <- doesFileExist filePath
--     if fileExists
--         then decodeByName <$> BL.readFile filePath
--         else return . Left $ printf "The file does not exist" filePath

-- --Remove the headers
-- removeHeader :: CSVData -> V.Vector HospitalDataset
-- removeHeader = snd

-- --Given a list, return only the elements with instrumentType "common stock"
-- filterStocks :: V.Vector HospitalDataset -> V.Vector HospitalDataset
-- filterStocks = filter isStock
--     where
--         isStock :: HospitalDataset -> Bool
--         isStock hospital = hospitalType hospital == "Common Stock"

-- --Read stocks from a CSV file
-- readStocks :: FilePath -> IO (Either ErrorMsg (V.Vector HospitalDataset))
-- readStocks filePath = 
--     (fmap . fmap) --lift the function twice
--         (filterStocks . removeHeader) --remove header and filter stocks
--             (parseCSV filePath) --read csv from filepath

-- Wrong code to be pushed to GitHub
-- data Category = Suspected | CovidPositive | NonCovid
--     deriving Show

-- individualByCategory :: Category -> HospitalDataset -> Integer
-- individualByCategory Suspected record = admitted_pui record
-- individualByCategory CovidPositive record = admitted_covid record
-- individualByCategory NonCovid record = admitted_total record - admitted_pui record - admitted_covid record 

-- instance FromNamedRecord (String, Integer) where
--     parseNamedRecord individualSuspected =
--        (,) <$> individualSuspected .: "state" <*> individualSuspected .: "admitted_pui"

-- extractSuspectedTuple :: HospitalDataset -> (String, Integer)
-- extractSuspectedTuple suspectedDataset = (state suspectedDataset, admitted_pui suspectedDataset)

-- createList :: V.Vector HospitalDataset -> Eq String => [(String, Integer)]
-- createList suspectedData = groupBy (== fst) V.toList $ V.map extractSuspectedTuple suspectedData

-- sumSuspected :: (String, Integer) -> Integer
-- sumSuspected secondTuple = V.sum $ V.map snd secondTuple

-- countState :: (String, Integer) -> Integer
-- countState firstTuple = length firstTuple

-- question3 :: IO ()
-- question3 = do
--     result <- readHospitalData "hospital.csv"
--     case result of
--         Left err -> putStrLn $ "Error Parsing CSV: " ++ err
--         Right hospitalData -> do
--             let suspectedTuple = extractSuspectedTuple hospitalData
--             let list = createList suspectedTuple
--             let sum = sumSuspected list
--             let count = countState list
--             let average = fromIntegral sum / fromIntegral count
--             putStrLn $ "Average: " ++ show average


-- data Category = Suspected | CovidPositive
--     deriving Show

-- individualByCategory :: Category -> HospitalDataset -> Integer
-- individualByCategory Suspected record = admitted_pui record
-- individualByCategory CovidPositive record = admitted_covid record

-- createList :: Category -> V.Vector HospitalDataset -> [(String, [Integer])]
-- createList category hospitalData = 
--     let tuples = V.toList $ V.map (\record -> (state record, individualByCategory category record)) hospitalData
--         in map (\group -> (state (head group), map snd group)) $
--         groupBy ((==) `on` state) tuples 

-- createTuple :: HospitalDataset -> (String, Integer)
-- createTuple avgdata = (state avgdata, admitted_pui avgdata)

-- createList :: V.Vector HospitalDataset -> [(String, Integer)]
-- createList tuple = V.toList $ V.map createTuple tuple

-- createTupleByState :: [(String, Integer)] -> [(String, [Integer])]
-- createTupleByState list = map (fst (head createList list), map snd createList list) $
--                             groupBy ((==) `on` state) $ sortOn state

-- sumSuspected :: [Integer] -> Integer
-- sumSuspected = sum

-- countState :: [Integer] -> Int
-- countState = length 

-- question3 :: IO ()
-- question3  = do
--     result <- readHospitalData "hospital.csv"
--     case result of
--         Left err -> putStrLn $ "Error Parsing CSV: " ++ err
--         Right hospitalData -> do
--             let tupleList = createList hospitalData
--             let tuplebyState = createTupleByState tupleList
--             let averages = map (\(st, counts) -> (st, fromIntegral (sumSuspected counts) / fromIntegral (countState counts))) tuplebyState
--             mapM_ (\(st, avg) -> putStrLn $ "State: " ++ st ++ ", Average: " ++ show avg) averages

-- createList :: Category -> V.Vector HospitalDataset -> [(String, [Integer])]
-- createList category hospitalData = 
--     let tuples = V.toList $ V.map (\record -> (state record, individualByCategory category record)) hospitalData
--         in map (\group -> (state (head group), map snd group)) $
--         groupBy ((==) `on` state) tuples 

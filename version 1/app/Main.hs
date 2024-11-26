{-# LANGUAGE OverloadedStrings #-} --Convert string literals to ByteString because cassava uses ByteString internally
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv --this is from the cassava library
import qualified Data.Vector as V
import Data.List (maximumBy, groupBy, sortOn)
import Data.Ord (comparing)
import Data.Ratio
import Data.Function (on)


-- Define the Date and Category types as before
type Date = String

data Category = Suspected | CovidPositive | NonCovid
    deriving (Eq, Show)

data Bed = Beds | BedsCovid | BedsNonCrit | All
    deriving (Eq, Show)

data State = Johor | Kedah | Kelantan | Melaka | NegeriSembilan | Pahang | Perak | Perlis | PulauPinang | Sabah | Sarawak | Selangor | Terengganu | KL | Labuan
    deriving (Eq, Show)

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
-- Extract admitted numbers by category
admittedByCategory :: Category -> HospitalDataset -> Integer
admittedByCategory Suspected record = admitted_pui record
admittedByCategory CovidPositive record = admitted_covid record

groupByState :: [HospitalDataset] -> [(String, [HospitalDataset])]
groupByState records =
    map (\group -> (state (head group), group))
        $ groupBy ((==) `on` state) $ sortOn state records

-- Compute averages for each category by state
computeAverages :: [(String, [HospitalDataset])] -> [(String, Double, Double)]
computeAverages groupedData =
    map computeAverage groupedData
  where
    computeAverage (st, records) =
        let count = fromIntegral (length records) :: Double
            totalSuspected = fromIntegral (sum $ map (admittedByCategory Suspected) records)
            totalCovid = fromIntegral (sum $ map (admittedByCategory CovidPositive) records)
        in (st, totalSuspected / count, totalCovid / count)

question3 :: IO ()
question3 = do
    result <- readHospitalData "hospital.csv"
    case result of
        Left err -> putStrLn $ "Error Parsing CSV: " ++ err
        Right hospitalData -> do
            let groupedData = groupByState (V.toList hospitalData)
            let averages = computeAverages groupedData
            putStrLn "State-wise Averages:"
            mapM_ print averages

main :: IO ()
main = do
    putStrLn $ "Functional Programming Language: "


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


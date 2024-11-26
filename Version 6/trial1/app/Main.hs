{-# LANGUAGE OverloadedStrings #-} --Convert string literals to ByteString because cassava uses ByteString internally
module Main where

-- Data.Bytestring.Lazy explanation (need to cabal install --lib bytestring in order to use it).
-- Use to read data as a sequence of binary data. Data.Bytestring.Lazy enables large data
-- to be processed efficiently by not loading all the large file data into memory directly.
-- Instead, it load the data into the memory only when needed to reduce stress on memory.
-- The import qualified means the imported modules functions, types and other definitions must be assessed with a prefix (BL in our code).
-- We cannot the module's functions and types directly by their name. 
import qualified Data.ByteString.Lazy as BL 
import Data.Csv --this is from the cassava library
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
readHospitalData :: FilePath -> IO (Either String ([HospitalDataset]))
readHospitalData filePath = do 
    csvData <- BL.readFile filePath
    let result = decodeByName csvData
    case result of
        Left err -> return $ Left err
        Right (_, records) -> return $ Right (foldr (:) [] records)


-- -- Code for question 1
-- Create tuple 2 for each states and their number of beds
extractTuple :: HospitalDataset -> (String, Integer)
extractTuple dataset = (state dataset, beds dataset)

processData :: [HospitalDataset] -> [(String, Integer)]
processData hospitalData = map extractTuple hospitalData

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
            let tuples = processData hospitalData
            let largestTuple = largestByBeds tuples
            let state = firstValueofTuple largestTuple
            putStrLn $ "State with the largest number of beds is: " ++ show state

-- ------------------------------------------------------Version 1-----------------------------------------
-- Question 2
-- Get total bed based on category for a HospitalData record
data Bed = Beds | BedsCovid | BedsNonCrit | All
    deriving (Eq, Show)

bedByCategory :: Bed -> HospitalDataset -> Integer
bedByCategory Beds record = beds record
bedByCategory BedsCovid record = beds_covid record
bedByCategory BedsNonCrit record = beds_noncrit record
bedByCategory All record = beds record + beds_covid record + beds_noncrit record

-- Sum admitted or discharged by category for all records
totalbedByCategory :: Bed -> [HospitalDataset] -> Integer
totalbedByCategory bed = sum . map (bedByCategory bed)

question2 :: IO ()
question2 = do
    result <- readHospitalData "hospital.csv"
    case result of
        Left err -> putStrLn $ "Error Parsing CSV: " ++ err
        Right hospitalData -> do
            let totalBedsCovid = totalbedByCategory BedsCovid hospitalData
            let totalBeds = totalbedByCategory Beds hospitalData
            let ratio = fromIntegral totalBedsCovid % fromIntegral totalBeds
            let num = numerator (ratio)
            let den = denominator (ratio)
            putStrLn $ "The ratio is: " ++ show num ++ ":" ++ show den

-- Question 3
data Category = Suspected | CovidPositive
    deriving Show

individualByCategory :: Category -> HospitalDataset -> Integer
individualByCategory Suspected record = admitted_pui record
individualByCategory CovidPositive record = admitted_covid record

createTupleByState :: [HospitalDataset] -> [(String, [HospitalDataset])]
createTupleByState list = 
    map (\group -> (state (head group), group)) $
        groupBy ((==) `on` state) $ sortOn state list

sumSuspected :: [HospitalDataset] -> Integer
sumSuspected sums = sum $ map (individualByCategory Suspected) sums

computeSuspectedAverage :: [(String, [HospitalDataset])] -> [(String, Double)]
computeSuspectedAverage suspectedData =  
    map computeAverage suspectedData
  where  
    computeAverage (st, records) =
        let count = fromIntegral (length records) :: Double
            totalSuspected = fromIntegral (sumSuspected records) :: Double
            average = totalSuspected / count
        in (st, average)

sumCovid :: [HospitalDataset] -> Integer
sumCovid sums = sum $ map (individualByCategory CovidPositive) sums

computeCovidAverage :: [(String, [HospitalDataset])] -> [(String, Double)]
computeCovidAverage covidData =  
    map computeAverage covidData
  where  
    computeAverage (st, records) =
        let count = fromIntegral (length records) :: Double
            totalCovid = fromIntegral (sumCovid records) :: Double
            average = totalCovid / count
        in (st, average)

question3 :: IO ()
question3  = do
    result <- readHospitalData "hospital.csv"
    case result of
        Left err -> putStrLn $ "Error Parsing CSV: " ++ err
        Right hospitalData -> do
            let list = createTupleByState (hospitalData)
            let suspectedavg = computeSuspectedAverage list
            let covidavg = computeCovidAverage list
            putStrLn $ "Average of Suspected individual in each states: "
            mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) suspectedavg
            putStrLn $ ""
            putStrLn $ "Average of Individual in Category Covid-19 Positive for each states: "
            mapM_ (\(st, avg) -> printf "%s: %.2f\n" st avg) covidavg

main :: IO ()
main = do
    question1
    putStrLn $ ""
    question2
    putStrLn $ ""
    question3
module Main where

import Data.List (maximumBy, groupBy, sortOn, elemIndex)
import Data.Ord (comparing)
import Data.Ratio
import Data.Function (on)
import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Char (toLower)



splitData :: [String] -> [[String]]
splitData = map (splitOn ",")

removeHeader :: [[String]] -> [[String]]
removeHeader = tail 

csvData :: IO [String]
csvData = lines <$> readFile "hospital.csv"

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

main :: IO ()
main = do
    beds_coviddataset <- beds_covid
    putStrLn $ show beds_coviddataset


-- -- -- Code for question 1
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

-- Question 2
-- Get total bed based on category for a HospitalData record
data Bed = Beds | BedsCovid | BedsNonCrit 
    deriving (Eq, Show)

bedByCategory :: Bed -> IO Integer
bedByCategory category = do
    bedslist <- case category of
        Beds -> beds
        BedsCovid -> beds_covid
        BedsNonCrit -> beds_noncrit
    return $ sum bedslist

bedsRatio :: IO String
bedsRatio = do
    totalBeds <- bedByCategory Beds
    totalBedsCovid <- bedByCategory BedsCovid
    let ratio = fromIntegral totalBeds % totalBedsCovid
    let num = numerator ratio
    let den = denominator ratio
    return $ show den ++ ":" ++ show num


question2 :: IO ()
question2 = do
    ratio <- bedsRatio
    putStrLn $ "The ratio is: " ++ ratio

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

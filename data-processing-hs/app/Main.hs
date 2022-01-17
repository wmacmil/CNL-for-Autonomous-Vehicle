{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- set -XOverloadedStrings

module Main where

-- import Utils.Misc
import Data.Maybe
import Data.Attoparsec
import Data.List
import GHC.Generics
import Data.Aeson -- as JSON
import Data.Aeson.Lens -- as JSON
import Control.Lens-- as JSON
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Data.Map as M
import qualified Data.List as D

jsonFile :: FilePath
jsonFile = "text/train.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

outputFileName :: FilePath
outputFileName = "text/justSentences.txt"

outputFileName' :: FilePath
outputFileName' = "text/justSentences.txt"


taggedCaseless :: FilePath
taggedCaseless = "text/caseless2.txt"

tagged :: FilePath
tagged = "text/taggedSentences.txt"

taggedBidirec :: FilePath
taggedBidirec = "text/bidectionalSentences.txt"


-- as suggested on IRC
parseMultipleJSON :: B.ByteString -> [Value]
parseMultipleJSON s = unfoldr (\s -> case parse json s of
                                  (Done s' o) -> Just (o,s');
                                  _ -> Nothing)
                      (B.toStrict s)

-- super duper ugly, how to do this without fromJust?
{-
  This function takes the json file with a full_text field and extracts the
  values from that field into a seperate text file
-}
-- >>> :t T.concat
-- T.concat :: [T.Text] -> T.Text
json2Sentences = do
  jsonFile <- getJSON
  let values          = parseMultipleJSON jsonFile
  let justTextEntries = fmap (^? key "full_text") values
  let textEntries     = map fromJust justTextEntries
  let justText        = map (^? _String) textEntries
  let text            = map fromJust justText :: [T.Text]
  let concatV         = T.intercalate " " text :: T.Text -- weird concat probs
  TIO.writeFile outputFileName' concatV

-- do
-- getWords :: FilePath -> IO [String]
-- so now we want to be able to do SQL type queries, as in sort by most frequent the followed by a given part of speech
-- would be nice to have Ngrams record which n in the type

-- what is the right abstraction?
-- lets say get the lists of ngrams, i.e. bigramFreqs below

-- count


getNGramWords n path =
  do
    contents <- readFile path
    let sents             = lines contents
        splitSents        = map words sents
        splitPOS          = fmap (fmap splitAtUnderscore) splitSents
        bigramFreqs       = concat $ map (ngrams n) splitPOS  :: [[(String,String)]]
    return bigramFreqs

twoGrams = getNGramWords 2 taggedCaseless
oneGrams = getNGramWords 1 taggedCaseless

-- 48 unigrams 

-- sortNGramFreqs :: IO [[(String,String)]] -> IO [[(([String], [String]), Int)]]
sortNGramFreqs ngramsM =
  do
    ngrams <- ngramsM
    let unzippedGrams = map unzip ngrams :: [([String],[String])]
    let sortedNGrams = sortBy (\(_,a) (_,b) -> compare a b)  unzippedGrams
    let groupedsortedNGrams = groupBy (\(_,a) (_,b) -> a == b) sortedNGrams :: [[([String], [String])]]
    let groupedFrequencies = map frequency groupedsortedNGrams
    let sortedGroupFreqs = map (\x -> reverse $ sortBy (\(_,a) (_,b) -> compare a b) x) groupedFrequencies
    -- return sortedNGrams
    -- return (map (D.take 3) sortedGroupFreqs) -- groupedsortedNGrams
    mapM putStrLn $ map show (map (D.take 5) sortedGroupFreqs) -- groupedsortedNGrams


-- one could sort by a given n gram, some kind of general pattern, like the _NN
-- or what is the most frequent verb

getOne2NGramWords n path =
  do
    contents <- readFile path
    let sents             = lines contents
    let splitSents        = map words sents
    let splitPOS          = fmap (fmap splitAtUnderscore) splitSents
    let oneTo5Grams = map ngrams [1..n]
    let bigramFreqs       = map concat $ map (\f -> map f splitPOS) oneTo5Grams
    let frequencies       = map frequency bigramFreqs
    let sortedFrequencies = map (\x -> reverse $ sortBy (\(_,a) (_,b) -> compare a b) x) frequencies
    return sortedFrequencies
    -- return (map (D.take 10) sortedFrequencies) -- to visualize

z = getOne2NGramWords 5 tagged
z' = getOne2NGramWords 5 taggedBidirec
z'' = getOne2NGramWords 5 taggedCaseless


splitAtUnderscore :: String -> (String,String)
splitAtUnderscore (x:xs)
  | x == '_' = ("",xs)
  | otherwise = cons2Pairs (x,[])  (splitAtUnderscore xs)-- (x, : splitAtUnderscoreHelper xs
  where
    cons2Pairs :: (x,[x]) -> ([x],[x]) -> ([x],[x])
    cons2Pairs (x,y) (xs,ys) = (x:xs,y ++ ys)

pos = ["CC","CD","CDZ","DT","EX","FW","IN","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NNSZ","NNZ","NP","NPS","NPSZ","NPZ","PDT","PP","PPZ","RB","RBR","RBS","RP","SENT","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","VH","VHD","VHG","VHN","VHP","VHZ","VV","VVD","VVG","VVN","VVP","VVZ","WDT","WP","WPZ","WRB","Z","#","$","\"","'","(",")",",",":"]


main :: IO ()
main = putStrLn "Hello, Haskell!"

-- ngrams
-- >>> ngrams 5 "ask"
-- ["ask"]
-- >>> ngrams 4 [1..4]
-- [[1,2,3,4]]

-- ngrams n xs = (D.take n xs) : ngrams n (tail xs)
-- isomorphic to boilerplate below
--
ngrams :: Int -> [a] -> [[a]]
ngrams n xs
  | (length xs) < n = [] -- [xs]
  | (length xs) == n = [xs]
  | otherwise = (D.take n xs) : ngrams n (tail xs)

startSymbol :: String
startSymbol = "START_START "

ngrams_Sent n x = ngrams n (("START","START"):x)

bigrams = ngrams_Sent 2

-- frequency
frequency :: (Eq a0, Ord a0) => [a0] -> [(a0, Int)]
frequency s = map (\x -> (head x, length x)) . group . sort $ s


-- myWords :: String -> Char -> String -> [String]
-- myWords (x:xs) c s =
--   | x == c = s

-- parts of speech shown here
-- https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

-- how to let these inform design of our programming language :
-- we should make sure that it parses n-grams of some large size, (which will allow us to build the corpus which matches best our corpus
-- i.e. write a helper function to GF to allow every
-- noun in the corpus to recieve a GF word
-- we could do this at the abstract sytnax level, or at the linearization level
-- if we just have a record for all the nouns one anticipates

-- write a words

--BOILERPLATE

-- bigrams :: [x] -> [(x,x)]
-- bigrams (x:y:xs) = (x,y) : (bigrams (y:xs))
-- bigrams _         = []

-- trigrams :: [x] -> [(x,x,x)]
-- trigrams (x:y:z:xs) = (x,y,z) : (trigrams (y:z:xs))
-- trigrams _         = []

-- quadgrams :: [x] -> [(x,x,x,x)]
-- quadgrams (x:y:z:z':xs) = (x,y,z,z') : (quadgrams (y:z:z':xs))
-- quadgrams _         = []

-- fivegrams :: [x] -> [(x,x,x,x,x)]
-- fivegrams (x:y:z:z':z'':xs) = (x,y,z,z',z'') : (fivegrams (y:z:z':z'':xs))
-- fivegrams _         = []

-- bigrams_Sent x = bigrams (startSymbol:x)
-- trigrams_Sent x = trigrams (startSymbol:x)
-- quadgrams_Sent x = quadgrams (startSymbol:x)
-- fivegrams_Sent x = fivegrams (startSymbol:x)

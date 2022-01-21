{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- set -XOverloadedStrings

module Main where

-- import Utils.Misc
import Data.Maybe
import Data.Attoparsec
import Data.List
import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as D
import qualified Data.Tuple.Extra as E

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
      justTextEntries = fmap (^? key "full_text") values
      textEntries     = map fromJust justTextEntries
      justText        = map (^? _String) textEntries
      text            = map fromJust justText :: [T.Text]
      concatV         = T.intercalate " " text :: T.Text -- weird concat probs
  TIO.writeFile outputFileName' concatV

-- 1. Get the tagged sentences line by line (roughtly 50k)
-- 2. Tokenize every line via whitespace (punctuation was already deal with POS tagger)
-- 3. Separate the words from the POS tags for every word on every line
tokenizeAndSplitPOS :: FilePath -> IO [[(String,String)]]
tokenizeAndSplitPOS path =
  let f = map (map splitAtUnderscore) . map words . lines
  in liftM f (readFile path)

-- could also ngramify this operation
-- maybe want to ngramify this first, but now we can search through examples
rawTokenizedSents = liftM f tokenizedPOSsents
  where f = map (map fst)

-- get the Ngrams for all the sentences
-- note that the sentence structure distinction has been with concat
nGramify :: Int -> IO [[(String,String)]] -> IO [[(String,String)]]
nGramify n = liftM (\s -> concat $ map (ngrams n) s)

tokenizedPOSsents = tokenizeAndSplitPOS taggedCaseless

oneGrams   = nGramify 1 tokenizedPOSsents
twoGrams   = nGramify 2 tokenizedPOSsents
threeGrams = nGramify 3 tokenizedPOSsents

-- >>> :t id
-- id :: a -> a

-- could do a better job of controlling for optimization/complexity
-- 1. Seperate word and POS Ngrams : [(Word,POS)] to into ([Word],[POS])
-- 2. Sort and group ngram pairs by the POS ngrams
--    a. extract the POS ngram information so its not duplicated everywhere
-- 3. find the most common ngrams for every POS-ngram
-- 4. get the most common word Ngrams for all POS-ngrams
-- 5. Return all given POS-ngrams sorted by ngram frequency in descending order
sortNGramFreqs :: IO [[(String,String)]] ->  IO [([([String], Int)], [String])]
sortNGramFreqs ngrams =
  let sortNGramFreqs' =
        map (reverse . sortBy (\(_,a) (_,b) -> compare a b) E.*** id)
        . map (frequency E.*** id)
        . map (map fst E.&&& snd . head) -- isolate POS
        . groupBy (\(_,a) (_,b) -> a == b)
        . sortBy (\(_,a) (_,b) -> compare a b)
        . map unzip
  in liftM sortNGramFreqs' ngrams

showTopMGrams m sortedGroupFreqs =
  do
    sortedGroupFreqs' <- sortedGroupFreqs
    (mapM putStrLn $ map show (map ((D.take m) E.*** id) sortedGroupFreqs'))

oneGramFreqs = sortNGramFreqs oneGrams
twoGramFreqs = sortNGramFreqs twoGrams
threeGramFreqs = sortNGramFreqs threeGrams

-- TODO : allow one to incquire verbs, maybe create a dictionary for key value pairs
-- Really need to just turn this into some kind of data structure so that one can analyze it better
-- also, rename types just to make everyhint more legible

-- probs delete, once some stuff is fixed
-- everything at once, not ideal
getOne2NGramWords n path =
  do
    contents <- readFile path
    let sents             = lines contents
        splitSents        = map words sents
        splitPOS          = fmap (fmap splitAtUnderscore) splitSents
        oneTo5Grams = map ngrams [1..n]
        bigramFreqs       = map concat $ map (\f -> map f splitPOS) oneTo5Grams
        frequencies       = map frequency bigramFreqs
        sortedFrequencies = map (\x -> reverse $ sortBy (\(_,a) (_,b) -> compare a b) x) frequencies
    -- return sortedFrequencies
    return (map (D.take 10) sortedFrequencies) -- to visualize

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

-- should datatype-ify this, and then add verbs, nouns, etc
pos = ["CC","CD","CDZ","DT","EX","FW","IN","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NNSZ","NNZ","NP","NPS","NPSZ","NPZ","PDT","PP","PPZ","RB","RBR","RBS","RP","SENT","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","VH","VHD","VHG","VHN","VHP","VHZ","VV","VVD","VVG","VVN","VVP","VVZ","WDT","WP","WPZ","WRB","Z","#","$","\"","'","(",")",",",":"]

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- >>> ngrams 4 [1..3]
-- []
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


-- parts of speech shown here
-- https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

-- how to let these inform design of our programming language :
-- we should make sure that it parses n-grams of some large size, (which will allow us to build the corpus which matches best our corpus
-- i.e. write a helper function to GF to allow every
-- noun in the corpus to recieve a GF word
-- we could do this at the abstract sytnax level, or at the linearization level
-- if we just have a record for all the nouns one anticipates

-- write a words

-- 48 unigrams
-- so now we want to be able to do SQL type queries, as in sort by most frequent the followed by a given part of speech
-- would be nice to have Ngrams record which n in the type

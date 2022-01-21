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
-- import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Data.Map as M
import qualified Data.List as D
-- import qualified Control.Monad as M

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

-- could do a better job of controlling for optimization/complexity, but not really important at the moment
-- 1. Seperate word and POS Ngrams : [(Word,POS)] to into ([Word],[POS])
-- 2. Sort and group ngram pairs by the POS ngrams
--    a. it is posible extract the POS ngram information
-- 3. find the most common ngrams for every POS-ngram
-- 4. get the most common word Ngrams for all POS-ngrams
-- 5. Return them in some way so that they can be anylized
-- sortNGramFreqs :: IO [[(String,String)]] -> IO [[(([String], [String]), Int)]]
sortNGramFreqs ngramsM =
  let f =
          map (\x -> reverse $ sortBy (\(_,a) (_,b) -> compare a b) x)
          . map frequency
          . groupBy (\(_,a) (_,b) -> a == b)
          . sortBy (\(_,a) (_,b) -> compare a b)
          . map unzip
  in liftM f ngramsM
  -- do
  --   ngrams <- ngramsM
  --   let unzippedGrams       = map unzip ngrams :: [([String],[String])]
  --       sortedNGrams        = sortBy (\(_,a) (_,b) -> compare a b)  unzippedGrams
  --       groupedsortedNGrams = groupBy (\(_,a) (_,b) -> a == b) sortedNGrams :: [[([String], [String])]]
  --       groupedFrequencies  = map frequency groupedsortedNGrams
  --       sortedGroupFreqs    = map (\x -> reverse $ sortBy (\(_,a) (_,b) -> compare a b) x) groupedFrequencies
  --   -- return sortedNGrams
  --   -- return (map (D.take 3) sortedGroupFreqs) -- groupedsortedNGrams

  --   mapM putStrLn $ map show (map (D.take 5) sortedGroupFreqs) -- for easy repl viewing

oneGramFreqs = sortNGramFreqs oneGrams
twoGramFreqs = sortNGramFreqs twoGrams
threeGramFreqs = sortNGramFreqs threeGrams

-- one could sort by a given n gram, some kind of general pattern, like the _NN
-- or what is the most frequent verb

-- everything at once, not ideal
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


-- [((["$"],["$"]),1)]
-- [((["\""],["''"]),32),((["'"],["''"]),12)]
-- [(([","],[","]),15844),((["/"],[","]),38),((["-"],[","]),9),((["..."],[","]),1),((["---"],[","]),1)]
-- [((["("],["-LRB-"]),789),((["["],["-LRB-"]),10)]
-- [(([")"],["-RRB-"]),784),(([":)"],["-RRB-"]),76),((["]"],["-RRB-"]),10),(([">"],["-RRB-"]),1)]
-- [((["."],["."]),49102),((["!"],["."]),240),((["?"],["."]),6)]
-- [(([";"],[":"]),118),((["--"],[":"]),40),(([":"],[":"]),34),((["-"],[":"]),12)]
-- [((["traffic.At"],["ADD"]),7),((["again.Just"],["ADD"]),2),((["turn.As"],["ADD"]),1),((["traffic.When"],["ADD"]),1),((["straight.At"],["ADD"]),1)]
-- [((["multi"],["AFX"]),5),((["semi"],["AFX"]),2),((["non"],["AFX"]),2),((["s"],["AFX"]),1),((["post"],["AFX"]),1)]
-- [((["and"],["CC"]),18343),((["but"],["CC"]),557),((["or"],["CC"]),142),((["And"],["CC"]),36),((["both"],["CC"]),20)]
-- [((["two"],["CD"]),1630),((["one"],["CD"]),1597),((["three"],["CD"]),520),((["3"],["CD"]),364),((["four"],["CD"]),299)]
-- [((["the"],["DT"]),57541),((["a"],["DT"]),23844),((["this"],["DT"]),4340),((["that"],["DT"]),2328),((["an"],["DT"]),1948)]
-- [((["There"],["EX"]),3375),((["there"],["EX"]),2409),((["THERE"],["EX"]),2),((["their"],["EX"]),1),((["THere"],["EX"]),1)]
-- [((["vu"],["FW"]),2),((["deja"],["FW"]),2),((["i.e."],["FW"]),1),((["etc."],["FW"]),1)]
-- [((["-"],["HYPH"]),1503),((["/"],["HYPH"]),392)]
-- [((["on"],["IN"]),23762),((["of"],["IN"]),15631),((["to"],["IN"]),13546),((["with"],["IN"]),11551),((["in"],["IN"]),6762)]
-- [((["next"],["JJ"]),6174),((["red"],["JJ"]),4451),((["white"],["JJ"]),3490),((["green"],["JJ"]),3355),((["first"],["JJ"]),3243)]
-- [((["more"],["JJR"]),200),((["smaller"],["JJR"]),79),((["lower"],["JJR"]),35),((["larger"],["JJR"]),35),((["closer"],["JJR"]),23)]
-- [((["closest"],["JJS"]),284),((["nearest"],["JJS"]),188),((["most"],["JJS"]),32),((["highest"],["JJS"]),24),((["farthest"],["JJS"]),22)]
-- [((["1"],["LS"]),79),((["3"],["LS"]),78),((["2"],["LS"]),78),((["4"],["LS"]),67),((["5"],["LS"]),1)]
-- [((["will"],["MD"]),6817),((["should"],["MD"]),3860),((["'ll"],["MD"]),1989),((["can"],["MD"]),718),((["may"],["MD"]),48)]
-- [((["*"],["NFP"]),3),(([":-)"],["NFP"]),1),((["..."],["NFP"]),1)]
-- [((["intersection"],["NN"]),13377),((["right"],["NN"]),11942),((["left"],["NN"]),8153),((["traffic"],["NN"]),5458),((["Touchdown"],["NN"]),5118)]
-- [((["Orient"],["NNP"]),1206),((["orange"],["NNP"]),623),((["white"],["NNP"]),272),((["street"],["NNP"]),255),((["way"],["NNP"]),222)]
-- [((["poles"],["NNPS"]),24),((["bushes"],["NNPS"]),11),((["stripes"],["NNPS"]),10),((["signs"],["NNPS"]),8),((["flowers"],["NNPS"]),6)]
-- [((["awnings"],["NNS"]),679),((["windows"],["NNS"]),649),((["letters"],["NNS"]),591),((["signs"],["NNS"]),578),((["doors"],["NNS"]),577)]
-- [((["all"],["PDT"]),386),((["quite"],["PDT"]),15),((["such"],["PDT"]),13),((["half"],["PDT"]),10),((["both"],["PDT"]),1)]
-- [((["'s"],["POS"]),135),((["'"],["POS"]),3),((["\8217s"],["POS"]),2),((["s"],["POS"]),2)]
-- [((["you"],["PRP"]),15976),((["You"],["PRP"]),4205),((["it"],["PRP"]),3338),((["yourself"],["PRP"]),2243),((["them"],["PRP"]),372)]
-- [((["your"],["PRP$"]),14946),((["its"],["PRP$"]),176),((["Your"],["PRP$"]),108),((["their"],["PRP$"]),42),((["his"],["PRP$"]),30)]
-- [((["right"],["RB"]),4433),((["straight"],["RB"]),4373),((["forward"],["RB"]),2937),((["left"],["RB"]),2905),((["so"],["RB"]),2439)]
-- [((["more"],["RBR"]),64),((["further"],["RBR"]),20),((["closer"],["RBR"]),14),((["longer"],["RBR"]),11),((["farther"],["RBR"]),11)]
-- [((["most"],["RBS"]),37),((["least"],["RBS"]),1)]
-- [((["down"],["RP"]),1545),((["up"],["RP"]),744),((["out"],["RP"]),206),((["around"],["RP"]),134),((["off"],["RP"]),106)]
-- [((["-"],["SYM"]),14),((["/"],["SYM"]),3),((["="],["SYM"]),2),((["\\"],["SYM"]),1),((["*"],["SYM"]),1)]
-- [((["to"],["TO"]),1960),((["To"],["TO"]),23),((["na"],["TO"]),2),((["TO"],["TO"]),1)]
-- [((["Please"],["UH"]),136),((["Hello"],["UH"]),132),((["please"],["UH"]),111),((["Right"],["UH"]),92),((["right"],["UH"]),20)]

-- [((["be"],["VB"]),5955),((["Go"],["VB"]),5524),((["Turn"],["VB"]),4256),((["see"],["VB"]),4233),((["turn"],["VB"]),4182)]

-- [((["left"],["VBD"]),254),((["came"],["VBD"]),62),((["started"],["VBD"]),47),((["made"],["VBD"]),47),((["were"],["VBD"]),41)]
-- [((["sitting"],["VBG"]),1966),((["going"],["VBG"]),1699),((["facing"],["VBG"]),1010),((["moving"],["VBG"]),853),((["passing"],["VBG"]),620)]
-- [((["left"],["VBN"]),1856),((["parked"],["VBN"]),676),((["painted"],["VBN"]),354),((["fenced"],["VBN"]),271),((["colored"],["VBN"]),149)]
-- [((["are"],["VBP"]),3815),((["'re"],["VBP"]),1214),((["reach"],["VBP"]),1110),((["get"],["VBP"]),780),((["come"],["VBP"]),775)]
-- [((["is"],["VBZ"]),12320),((["has"],["VBZ"]),962),((["'s"],["VBZ"]),655),((["looks"],["VBZ"]),102),((["ends"],["VBZ"]),95)]

-- [((["that"],["WDT"]),1655),((["which"],["WDT"]),320),((["Which"],["WDT"]),2),((["whichever"],["WDT"]),1),((["what"],["WDT"]),1)]
-- [((["what"],["WP"]),64),((["who"],["WP"]),2),((["What"],["WP"]),1)]
-- [((["whose"],["WP$"]),2)]
-- [((["when"],["WRB"]),1075),((["When"],["WRB"]),1064),((["where"],["WRB"]),749),((["Where"],["WRB"]),7),((["y"],["WRB"]),1)]
-- [((["\""],["``"]),37),((["'"],["``"]),8)]
-- [(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]


-- >>> 3 + 4
-- 7

-- >>> z''

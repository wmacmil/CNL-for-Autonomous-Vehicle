{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- set -XOverloadedStrings

module Data where

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

-- import qualified Data.ByteString.Internal as I
-- import qualified Data.ByteString.Lazy.UTF8 as U

-- -- import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
jsonFile = "train.json"

outputFileName :: FilePath
outputFileName = "justSentences.txt"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

b' :: B.ByteString -> [Value]
b' s = unfoldr (\s -> case parse json s of (Done s' o) -> Just (o,s'); _ -> Nothing) (B.toStrict s)

-- how to do this without fromJust
b'' = do
  s' <- getJSON
  let x = b' s'
  let y = fmap (^? key "full_text") x
  let z = map fromJust y
  let w = map (^? _String) z
  let v = map fromJust w :: [T.Text]
  let concatV = T.concat v :: T.Text
  TIO.writeFile outputFileName concatV

-- >>> D.take 4 [1..10]
-- [1,2,3,4]
-- >>> D.tail [1..10]
-- [2,3,4,5,6,7,8,9,10]

-- ngrams

-- >>> ngrams 5 "ask"
-- ["ask"]
-- >>> ngrams 4 [1..4]
-- [[1,2,3,4]]

-- ngrams n xs = (D.take n xs) : ngrams n (tail xs)
-- isomorphic
ngrams :: Int -> [a] -> [[a]]
ngrams n xs
  | (length xs) < n = [] -- [xs]
  | (length xs) == n = [xs]
  | otherwise = (D.take n xs) : ngrams n (tail xs)

startSymbol = "_START "

ngrams_Sent n x = ngrams n (startSymbol:x)

bigrams = ngrams_Sent 2

-- frequency
frequency :: (Eq a0, Ord a0) => [a0] -> [(a0, Int)]
frequency s = map (\x -> (head x, length x)) . group . sort $ s

tagged :: FilePath
tagged = "taggedSentences.txt"

taggedBidirec :: FilePath
taggedBidirec = "bidectionalSentences.txt"

-- do
-- getWords :: FilePath -> IO [String]
getWords path =
  do
    contents <- readFile path
    let sents = lines contents
    let splitSents = map words sents
    let bigramFreqs = concat $ map bigrams splitSents
    let frequencies = frequency bigramFreqs
    let sortedFrequencies = reverse $ sortBy (\(_,a) (_,b) -> compare a b) frequencies
    return (D.take 50 sortedFrequencies)

z = getWords tagged
z' = getWords taggedBidirec

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



-- >>> bg = bigrams "askldfjas;ldkjasdl;fkajklweqrjklasdlk;jasld;fjasdlk;fjasd;lfkjasdl;fkjasdfl;kasdfl;asdjkfals;kdfas;ldkfjas;lkdjasdkasdfasdfasdfjkasldfjasld;kfjasdlfj"
-- <interactive>:7500:7-13: error:
--     • Variable not in scope: bigrams :: [Char] -> t
--     • Perhaps you meant ‘ngrams’ (line 64)
-- >>> frequency bg
-- <interactive>:7501:12-13: error:
--     • Variable not in scope: bg :: [a0]
--     • Perhaps you meant ‘b'’ (line 36)

-- >>> ff = (let f = frequency "asdjkfl;asfjdlsksjldkfljasdjkl" in f ++ f)
-- >>> map ff
-- [(";",1),("a",3),("d",4),("f",3),("j",5),("k",4),("l",5),("s",5),(";",1),("a",3),("d",4),("f",3),("j",5),("k",4),("l",5),("s",5)]
-- >>> frequency (let f = frequency "asdjkfl;asfjdlsksjldkfljasdjkl" in f ++ f)
-- [([(";",1)],2),([("a",3)],2),([("d",4)],2),([("f",3)],2),([("j",5)],2),([("k",4)],2),([("l",5)],2),([("s",5)],2)]

-- need to pre-append StartSentence tag
-- approximately 45000 sentences
-- idea : run bigram and trigam data per sentence,
-- then sort over set of all bigrams and trigrams
-- construct a map

-- constructMap :: [x] -> Map x Int

-- updateValOrConstuctKey :: (Ord x) => x -> M.Map x Int -> M.Map x Int
-- updateValOrConstuctKey x m =
--   let queryXM = M.lookup x m
--   in
--     if M.member x m
--       then M.insert x 1 m
--       else M.adjust (+1) x m

    -- case queryXM of
    --   Nothing -> M.insert x 1 m
    --   Just something -> _


-- or just build a dictionary between a list of items and its count
-- >>> sort [("abc","bc"),("cb","df"),("bd,"bd")]
-- <interactive>:1545:44: error:
--     lexical error in string/character literal at end of input














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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- set -XOverloadedStrings

module Data where

import Data.Maybe
import Data.Attoparsec
import Data.List
import GHC.Generics
import Data.Aeson -- as JSON
import Data.Aeson.Lens -- as JSON
import Control.Lens-- as JSON
-- import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Data.ByteString.Internal as I
-- import qualified Data.ByteString.Lazy.UTF8 as U

-- -- import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
-- jsonFile = "train.json"
jsonFile = "train.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- a = fmap decode getJSON :: 

s = "{ \"foo\": \"bar\", \"baz\": 42 }\n{ \"foo\": \"freak\", \"baz\": 11112 }"

-- v = decode "{ \"foo\": \"bar\", \"baz\": 42 }" :: Maybe Value
-- v' :: _ -> Maybe Value

v' v = v >>= (^? key "foo")

-- b = (\s -> case parse json s of (Done s' o) -> Just (o,s'); _ -> Nothing) s

-- b :: Internal.ByteString -> Maybe (Value, Data.ByteString.Internal.ByteString)

b :: [Value]
b = unfoldr (\s -> case parse json s of (Done s' o) -> Just (o,s'); _ -> Nothing) s


-- -- b' :: B.ByteString -> [MaybeValue]
-- aa' s = unfoldr decode s

b' :: B.ByteString -> [Value]
b' s = unfoldr (\s -> case parse json s of (Done s' o) -> Just (o,s'); _ -> Nothing) (B.toStrict s)

asdf v = v >>= (^? key "foo") >>= (^? _String)

-- >>> :t (^? _String)
-- (^? _String) :: AsPrimitive s => s -> Maybe Data.Text.Internal.Text

outputFileName :: FilePath
outputFileName = "justSentences.txt"

-- b'' :: IO B.ByteString -> IO [Value]

-- b'' :: Monad m => m B.ByteString -> m [T.Text]
b'' = do
  s' <- getJSON
  -- s' <- s
  let x = b' s'
  let y = fmap (^? key "full_text") x
  let z = map fromJust y
  let w = map (^? _String) z
  let v = map fromJust w :: [T.Text]
  let concatV = T.concat v :: T.Text
  TIO.writeFile outputFileName concatV
  -- return ()

-- writeListText :: [T.Text] -> IO ()
-- writeListText  = _

c = b'' 

-- main = printAnswer =<< pure . programLogic =<< readFile "name"

-- b''' :: IO a -> (a -> [b]) -> [b]
-- b''' ioa f = do
--   a <- ioa
--   _

-- >>> :t (<<=)
-- <interactive>:1:1-5: error:
--     • Variable not in scope: <<=
--     • Perhaps you meant ‘<=’ (imported from Prelude)

result = parse json s

-- result' = do
--   train <- getJSON
--   b' train
--   _
--   -- b' train

-- >>> :t unfoldr
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- >>> b
-- [Object (fromList [("foo",String "bar"),("baz",Number 42.0)]),Object (fromList [("foo",String "freak"),("baz",Number 11112.0)])]

-- repeatb s = b s




-- >>> b s
-- <interactive>:2444:2-4: error:
--     • Couldn't match expected type ‘Data.ByteString.Internal.ByteString
--                                     -> t’
--                   with actual type ‘[Value]’
--     • The function ‘b’ is applied to one argument,
--       but its type ‘[Value]’ has none
--       In the expression: b s
--       In an equation for ‘it’: it = b s
--     • Relevant bindings include it :: t (bound at <interactive>:2444:2)

-- "{\"city\": \"nyc\", \"full_text\": \"Follow traffic to the light and go left, then left again at the next light. Don't cross the cross way, but turn to look left at the blue-grey building with the red awning.  There's a black hydrant with a silver cap between two silver bollards on the sidewalk.\\n\\nTouchdown is on the cap of the hydrant.\"}\n{\"city\": \"nyc\", \"full_text\": \"Turn so the nearby intersection is at your back, and the building with the murals is on your right.  Go forward, and move straight through the first two intersections you come to.  Now the median strip won't have plants and a green stripe in it on this block.  Go forward, and look for a closed garage/security gate on your right side that is black with a yellow mural of a face on it.  Stop when you've just passed that mural.  The bear is on the right eye of the face in the mural. \"}\n\n"


-- -- trainLists :: _
-- trainLists =
--   do train <- getJSON
--      decode train -- :: (Maybe Value)-- getJSON 
--   -- do train <- getJSON
--   --    decode <$> getJSON -- getJSON 

-- data Routes = Routes
--   {
--     navigation_text :: String
--   }
--   deriving (Eq, Show, Generic)

-- instance FromJSON Routes where
--   parseJSON = genericParseJSON $ jsonOptions

-- jsonOptions :: Options
-- jsonOptions = defaultOptions

-- trainLists :: IO (Either String [Routes])
-- trainLists =
--   do -- train <- getJSON
--      eitherDecode <$> getJSON -- getJSON 

     -- _
     -- let x = U.toString train
     -- -- let x = B.unpack train
     -- putStrLn x

-- getText :: [Routes]
-- getText = do
--   trainData <- getJSON
--   _

-- -- toString from Data.ByteString.Lazy.UTF8
-- main = do
--   trainData <- getJSON
--   _



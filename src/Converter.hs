module Converter where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSC
import Crypto.Hash
import Data.Word
import Data.Binary

bs2Int :: ByteString -> Word64
bs2Int = decode

md5 :: ByteString -> Digest MD5
md5 = hashlazy

get64bitHash :: ByteString -> ByteString
get64bitHash str = BS.take 8 $ fromChunks [h]
  where h = digestToByteString $ md5 str

get64bitId :: String -> Word64
get64bitId = bs2Int . get64bitHash . BSC.pack

processLines :: [String] -> [(Word64, String)]
processLines = Prelude.map (\str -> (get64bitId str, str))

pairToStr :: (Word64, String) -> String
pairToStr (num, str) = (show num) ++ ' ' : str



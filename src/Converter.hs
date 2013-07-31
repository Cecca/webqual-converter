module Converter (
    -- * Hash conversions
      bs2Int, get64bitHash, get64bitId,
    -- * Urls file processing
      processUrls,
    -- * Links file processing
      processLinks
    ) where

import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 as BSC
import Crypto.Hash
import Data.Word
import Data.Binary
import Data.Bits
import Control.Parallel.Strategies

-- | Convert a byteString to an unsigned 64 bit integer
bs2Int :: ByteString -> Word64
bs2Int = decode

-- | Computes the MD5 hash of a bytestring
md5 :: ByteString -> Digest MD5
md5 = hashlazy

-- | Extracts the 64 bit hash from a 128 MD5 hash
get64bitHash :: ByteString -> ByteString
get64bitHash str = clearBit first 7 `BS.cons` rest
  where Just (first,rest) = BS.uncons signedHash
        signedHash = BS.take 8 $ fromChunks [h]
        h = digestToByteString $ md5 str

-- | Extracts the unsigned 64 bit integer hash from a 128 bit one
get64bitId :: ByteString -> Word64
get64bitId = bs2Int . get64bitHash

-- # Urls file processing #

processUrls :: ByteString -> ByteString
processUrls = BSC.unlines . processLines . BSC.lines

-- | Associate to each bytestring its 64 bit integer hash
processLines :: [ByteString] -> [ByteString]
processLines = parMap rseq (\str -> pairToStr (get64bitId str, str))

-- | Converts a pair of hash and associated string to a single bytestring,
-- with a space between the two values
pairToStr :: (Word64, ByteString) -> ByteString
pairToStr (num, str) = numStr `BSC.append` sp `BSC.append` str
    where sp = BSC.singleton ' '
          numStr = BSC.pack . show $ num

-- # Links file processing #

processLinks :: ByteString -> ByteString
processLinks = unGroupHashes . (parMap rseq get64bitHash) . groupHashes

-- | Given a bytestring, groups the bytes 16 by 16
groupHashes :: ByteString -> [ByteString]
groupHashes bs
    | BS.null bs = []
    | otherwise = BS.take 16 bs : groupHashes (BS.drop 16 bs)

-- | Concatenates a list of bytestrings into a single one
unGroupHashes :: [ByteString] -> ByteString
unGroupHashes = BS.concat


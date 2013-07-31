import Criterion.Main
import Criterion.Config
import Converter
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BS
import Crypto.Hash

main :: IO ()
main = defaultMainWith defaultConfig (return ()) [
          bgroup "processUrls" [
            bench "processUrls 1000" $ whnf processUrls (lns 1000),
            bench "processUrls 2000" $ whnf processUrls (lns 2000),
            bench "processUrls 3000" $ whnf processUrls (lns 3000),
            bench "processUrls 4000" $ whnf processUrls (lns 4000),
            bench "processUrls 5000" $ whnf processUrls (lns 5000),
            bench "processUrls 6000" $ whnf processUrls (lns 6000)
          ],
          bgroup "processLinks" [
            bench "processLinks 10k" $ whnf processLinks (links 10000),
            bench "processLinks 20k" $ whnf processLinks (links 20000),
            bench "processLinks 30k" $ whnf processLinks (links 30000),
            bench "processLinks 40k" $ whnf processLinks (links 40000),
            bench "processLinks 50k" $ whnf processLinks (links 50000),
            bench "processLinks 60k" $ whnf processLinks (links 60000),
            bench "processLinks 70k" $ whnf processLinks (links 70000)
          ]
        ]

lns :: Int -> BSC.ByteString
lns n = BSC.concat $ take n $ cycle [BSC.pack "line"]

links :: Int -> BS.ByteString
links n = BS.concat $ take n $ cycle [h]
    where h = BS.fromChunks [h']
          h' = digestToByteString $ md5 $ BSC.pack "stringToDigest"

md5 :: BS.ByteString -> Digest MD5
md5 = hashlazy


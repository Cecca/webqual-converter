import Criterion.Main
import Criterion.Config
import Converter
import qualified Data.ByteString.Lazy.Char8 as BSC

main :: IO ()
main = defaultMainWith defaultConfig (return ()) [
          bgroup "processUrls" [
            bench "processUrls 1000" $ whnf processUrls (lns 1000),
            bench "processUrls 2000" $ whnf processUrls (lns 2000),
            bench "processUrls 3000" $ whnf processUrls (lns 3000),
            bench "processUrls 4000" $ whnf processUrls (lns 4000),
            bench "processUrls 5000" $ whnf processUrls (lns 5000),
            bench "processUrls 6000" $ whnf processUrls (lns 6000)
          ]
        ]

lns :: Int -> BSC.ByteString
lns n = BSC.concat $ take n $ cycle [BSC.pack "line"]


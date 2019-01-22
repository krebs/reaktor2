module Reaktor.Nick where

import Data.ByteString.Char8.Extended (ByteString)
import qualified Data.ByteString.Char8.Extended as BS
import           Data.Char (chr)
import           Data.Char (isDigit)
import           System.Random (getStdRandom, randomR)


getNext :: ByteString -> ByteString
getNext nick_ = nick'
  where
    splitNick s =
           (prefix, maybe 0 fst (BS.readInt suffix))
         where
           prefix = BS.take (BS.length s - BS.length suffix) s
           suffix = BS.reverse . BS.takeWhile isDigit . BS.reverse $ s
    (nickPrefix, nickSuffix) = splitNick nick_
    nick' = nickPrefix <> (BS.pack . show $ nickSuffix + 1)


getRandom :: IO ByteString
getRandom = do
    h_chr <- getRandomChar nickhead
    t_len <- getStdRandom (randomR (4,8)) :: IO Int
    t_str <- mapM (const $ getRandomChar nicktail) [1..t_len]
    return $ BS.pack (h_chr:t_str)
  where
    getRandomChar cs = (cs!!) <$> getStdRandom (randomR (0, length cs - 1))

    -- RFC2812 (doesn't work with charybdis)
    --nickhead  = letters <> specials
    --nicktail  = letters <> digits <> specials <> minus
    --letters   = map chr $ [0x41..0x5A] <> [0x61..0x7A]
    --digits    = map chr $ [0x30..0x39]
    --specials  = map chr $ [0x5B..0x60] <> [0x7B..0x7D]
    --minus     = map chr $ [0x2D]

    -- RFC1459
    nickhead  = letters
    nicktail  = letters <> number <> special
    letters   = map chr $ [0x41..0x5A] <> [0x61..0x7A]
    number    = map chr $ [0x30..0x39]
    special   = map chr $ [0x5B..0x60] <> [0x7B..0x7D] <> [0x2D]

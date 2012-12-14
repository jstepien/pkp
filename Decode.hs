module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.Word (Word8, Word32)
import Data.Array.Unboxed

decode :: ByteString -> [Word8]
decode contents = concatMap (toRGBA . (palette !)) $ unpack pixelData
  where
    toRGBA :: Word32 -> [Word8]
    toRGBA x = map (fi . (.&. 0xff)) [x, x .>>. 8, x .>>. 16, x .>>. 24]
    toWord r g b a = fi r + (fi g .<<. 8) + (fi b .<<. 16) + (fi a .<<. 24)
    bytes = B.unpack contents
    cols = 1 + fi (head bytes) :: Int
    fi a = fromIntegral a
    palette = array (0, fi cols - 1) colPairs :: UArray Word8 Word32
    paletteBytes = 4 * cols + 1
    pixelData = drop paletteBytes bytes
    colPairs = getColours 0 $ tail bytes
    getColours n (r:g:b:a:rest)
      | n /= cols = (fi n, toWord r g b a) : getColours (n + 1) rest
      | otherwise = []
    getColours _ _ = error "Invalid data"
    unpack :: [Word8] -> [Word8]
    unpack
      | cols <= 2  = unp 0x1 [0 .. 7]
      | cols <= 4  = unp 0x3 [0, 2, 4, 6]
      | cols <= 16 = unp 0xf [0, 4]
      | otherwise  = id
    unp mask offs = concatMap $ \b -> map ((.&. mask) .  (b .>>.)) offs
    a .>>. b = shiftR a b
    a .<<. b = shiftL a b

main :: IO ()
main = do
  bs <- B.getContents
  B.putStr $ B.pack $ decode $ B.drop 2 bs

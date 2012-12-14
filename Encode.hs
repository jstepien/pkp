module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.Environment (getArgs)
import Data.Bits
import Data.Word (Word8)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Array.Unboxed
import Data.List
import Data.Function (on)

err :: a
err = error "Internal encoding error"

type Palette = IntMap Word8
type Source = UArray Int Word8
type Order = [Int]

encode :: Palette -> Source -> Order -> [Word8]
encode pal source = map enc
  where
    enc idx = case IntMap.lookup (k $ 4 * idx) pal of
                Just val -> val
                Nothing  -> err
    k i = mkKey (source ! (i + 0)) (source ! (i + 1))
                (source ! (i + 2)) (source ! (i + 3))
    {-# INLINE k #-}

maxPaletteSize :: Int
maxPaletteSize = 256

mkKey :: Word8 -> Word8 -> Word8 -> Word8 -> Int
mkKey r g b a = r' + (g' << 8) + (b' << 16) + (a' << 24)
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b
    a' = fromIntegral a
    (<<) = shift
{-# INLINE mkKey #-}

palette :: Palette -> Source -> Int -> Int -> Palette
palette pal source beg end
  | beg == end = pal
  | otherwise  = case entry of
    (Just _) -> palette pal source (beg + 1) end
    Nothing  -> palette (IntMap.insert key (fromIntegral $ IntMap.size pal) pal)
                        source (beg + 1) end
    where
      entry = IntMap.lookup key pal
      i = 4 * beg
      key = mkKey (source ! (i + 0)) (source ! (i + 1))
                  (source ! (i + 2)) (source ! (i + 3))
      {-# INLINE entry #-}
      {-# INLINE i #-}
      {-# INLINE key #-}

encPallete :: Palette -> IO ()
encPallete pal = do
  B.putStr $ B.pack [fromIntegral $ IntMap.size pal - 1]
  (mapM_ enc . sortBy (compare `on` snd) . IntMap.assocs) pal
  where
    enc (col, _) = B.putStr $ B.pack $ asBytes col
    asBytes x = map (fromIntegral . (.&. 0xff) . shiftR x) [0, 8, 16, 24]

fmtWidth :: Int -> ByteString
fmtWidth width = B.pack $ map fromIntegral bytes
  where
    bytes = [ (width `shiftR` 8) .&. 0xff
            , width .&. 0xff]

printBytes :: Int -> [Word8] -> IO ()
printBytes _ [] = return ()
printBytes cols bs = let (chunk, rest) = splitAt 64 bs in do
  B.putStr $ B.pack $ pack chunk
  printBytes cols rest
  where
    pack
      | cols <= 2  = p8
      | cols <= 4  = p4
      | cols <= 16 = p2
      | otherwise  = id
    p2 (a:b:rest) = p [a, b] [0, 4] : pack rest
    p2 [] = []
    p2 _ = err
    p4 (a:b:c:d:rest) = p [a, b, c, d] [0, 2, 4, 6] : pack rest
    p4 [] = []
    p4 _ = err
    p8 (a:b:c:d:e:f:g:h:rest) = p [a, b, c, d, e, f, g, h] [0 .. 7] : pack rest
    p8 [] = []
    p8 _ = err
    p bytes offs = foldl1 (.|.) $ zipWith (.<<.) bytes offs
    (.<<.) = shiftL

loadPixels :: IO Source
loadPixels = do
  bs <- B.getContents
  return $ arr (B.length bs) bs
  where
    arr n bs = array (0, n - 1) $ zip [0 .. n - 1] $ B.unpack bs

horizontalOrder :: Int -> Int -> [Int]
horizontalOrder _ pixels = [0 .. pixels - 1]

main :: IO ()
main = do
  (w:_) <- getArgs
  let width = read w :: Int
  B.putStr $ fmtWidth width
  arr <- loadPixels
  let pixelCount = fromIntegral $ (snd (bounds arr) + 1) `div` 4
  let pal = palette IntMap.empty arr 0 pixelCount
  let colNum = IntMap.size pal
  if colNum > maxPaletteSize
    then error $ "za dużo kolorów: " ++ show colNum
    else do
      encPallete pal
      printBytes colNum $ encode pal arr (order width pixelCount)
        where order = horizontalOrder

module Data.Helpers(word16ToBS, word8ToBS, boolToWord8) where

import Data.ByteString as BS
import Data.Word
import Data.Bits

word16ToBS:: Word16 -> BS.ByteString
word16ToBS w = let a = fromIntegral (w `shiftR` 8) :: Word8 
                   b = fromIntegral (w .&. 0xFF) :: Word8 in
                   BS.pack [a, b]

word8ToBS:: Word8 -> BS.ByteString
word8ToBS w = BS.pack [w]

boolToWord8:: Bool -> Word8
boolToWord8 b = if b then 1 else 0

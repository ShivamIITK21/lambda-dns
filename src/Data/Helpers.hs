module Data.Helpers(word32ToBS, word16ToBS, word8ToBS, boolToWord8, serializeQName) where

import Data.ByteString as BS
import Control.Monad(guard)
import Data.ByteString.Char8 as BSC
import Data.List.Split
import Data.Word
import Data.Bits

word32ToBS:: Word32 -> BS.ByteString
word32ToBS w = let a = fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8
                   b = fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8
                   c = fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8
                   d = fromIntegral ((w `shiftR` 0) .&. 0xFF) :: Word8
               in BS.pack [a, b, c, d]


word16ToBS:: Word16 -> BS.ByteString
word16ToBS w = let a = fromIntegral (w `shiftR` 8) :: Word8 
                   b = fromIntegral (w .&. 0xFF) :: Word8 in
                   BS.pack [a, b]

word8ToBS:: Word8 -> BS.ByteString
word8ToBS w = BS.pack [w]


boolToWord8:: Bool -> Word8
boolToWord8 b = if b then 1 else 0



serializeQNameFromList:: [String] -> Maybe BS.ByteString
serializeQNameFromList [] = return (word8ToBS 0)
serializeQNameFromList (s:ss) = do 
                                guard (Prelude.length s <= 63)
                                rest <- serializeQNameFromList ss
                                return (BS.concat [label_len, string_bytes, rest])
                                    where label_len = word8ToBS (fromIntegral (Prelude.length s) :: Word8)
                                          string_bytes = BSC.pack s

serializeQName:: String -> Maybe BS.ByteString
serializeQName s = do 
                    let split_list = splitOn "." s in serializeQNameFromList split_list

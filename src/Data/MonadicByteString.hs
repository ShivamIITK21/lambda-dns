module Data.MonadicByteString(readWords, read16bit, read32bit, readQName) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word
import Data.Bits
import Control.Monad(guard, when)
import Control.Monad.State


readWords::BS.ByteString -> Int -> StateT Int Maybe [Word8] 
readWords bs n = do
                    offset <- get
                    guard (offset + n <= BS.length bs)
                    let readBytes = BS.take n (BS.drop offset bs)
                    put (offset + BS.length readBytes)
                    return (BS.unpack readBytes)

read16bit::BS.ByteString -> StateT Int Maybe Word16
read16bit bs = do
                offset <- get
                guard(offset + 2 <= BS.length bs)
                let b1 = fromIntegral (BS.index bs offset) :: Word16     -- Get first byte
                let b2 = fromIntegral (BS.index bs (offset+1)) :: Word16 -- Get second byte
                let val = shiftL b1 8 .|. b2
                put (offset + 2)
                return val

read32bit::BS.ByteString -> StateT Int Maybe Word32 
read32bit bs = do
                offset <- get
                guard(offset + 4 <= BS.length bs)
                do
                    let b1 = fromIntegral (BS.index bs offset) :: Word32 
                    let b2 = fromIntegral (BS.index bs (offset+1)) :: Word32
                    let b3 = fromIntegral (BS.index bs (offset+2)) :: Word32  
                    let b4 = fromIntegral (BS.index bs (offset+3)) :: Word32 
                    let val = (shiftL b1 24) .|. (shiftL b2 16) .|. (shiftL b3 8) .|. (b4)
                    put (offset + 4)
                    return val

readAt::BS.ByteString -> Int -> Int -> StateT Int Maybe BS.ByteString
readAt bs pos len = do 
                        guard(pos + len <= BS.length bs)
                        return (BS.take len (BS.drop pos bs))

readQNameUtil::BS.ByteString -> Int -> Bool ->  StateT Int Maybe String
readQNameUtil bs pos jumped = let byte = BS.index bs pos in 
                                    if (byte .&. 0xC0) /= 0xC0 then
                                        if byte == 0 then 
                                            do 
                                                when (not jumped) (put (pos + 1)) -- from Control.Monad
                                                return ""
                                        else
                                            do
                                                let chunkLen = fromIntegral byte
                                                chunk_bs <- readAt bs (pos + 1) chunkLen
                                                let chunk = BSC.unpack chunk_bs
                                                rest <- readQNameUtil bs (pos + 1 + chunkLen) jumped
                                                if not (null rest) then return (chunk ++ ['.'] ++ rest)
                                                else return chunk
                                        
                                    else
                                        if not jumped then
                                            do
                                                put (pos + 2)
                                                let secondByte = fromIntegral (BS.index bs (pos + 1)) :: Word16
                                                let len = fromIntegral byte :: Word16
                                                let offset = fromIntegral (((len .^. 0xC0) `shiftL` 8) .|. secondByte) :: Int
                                                readQNameUtil bs offset True
                                        else 
                                            do
                                                let secondByte = fromIntegral (BS.index bs (pos + 1)) :: Word16
                                                let len = fromIntegral byte :: Word16
                                                let offset = fromIntegral (((len .^. 0xC0) `shiftL` 8) .|. secondByte) :: Int
                                                readQNameUtil bs offset True

readQName::BS.ByteString -> StateT Int Maybe String
readQName bs = do
                pos <- get
                readQNameUtil bs pos False
                                                

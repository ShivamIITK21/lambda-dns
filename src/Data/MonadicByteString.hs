module Data.MonadicByteString(readWords, read16bit, read32bit, readQName) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word
import Data.Bits
import Control.Monad(when)
import Control.Monad.State


readWords::BS.ByteString -> Int -> State Int [Word8] 
readWords bs n = do
                    offset <- get
                    if offset + 1 >= BS.length bs then error "Nothing More to Read!"
                    else do 
                        let readBytes = BS.take n (BS.drop offset bs)
                        put (offset + BS.length readBytes)
                        return (BS.unpack readBytes)

read16bit::BS.ByteString -> State Int Word16 
read16bit bs = do
                offset <- get
                if offset + 1 >= BS.length bs then error "Can't read 2 bytes"
                else do
                    let b1 = fromIntegral (BS.index bs offset)     -- Get first byte
                    let b2 = fromIntegral (BS.index bs (offset+1)) -- Get second byte
                    let val = shiftL b1 8 .|. b2
                    put (offset + 2)
                    return val

read32bit::BS.ByteString -> State Int Word32 
read32bit bs = do
                offset <- get
                if offset + 3 >= BS.length bs then error "Can't read 4 bytes"
                else do
                    let b1 = fromIntegral (BS.index bs offset) :: Word32 
                    let b2 = fromIntegral (BS.index bs (offset+1)) :: Word32
                    let b3 = fromIntegral (BS.index bs (offset+2)) :: Word32  
                    let b4 = fromIntegral (BS.index bs (offset+3)) :: Word32 
                    let val = (shiftL b1 24) .|. (shiftL b2 16) .|. (shiftL b3 8) .|. (b4)
                    put (offset + 4)
                    return val

readAt::BS.ByteString -> Int -> Int -> BS.ByteString
readAt bs pos len = BS.take len (BS.drop pos bs)

readQNameUtil::BS.ByteString -> Int -> Bool ->  State Int String
readQNameUtil bs pos jumped = let byte = BS.index bs pos in 
                                    if (byte .&. 0xC0) /= 0xC0 then
                                        if byte == 0 then 
                                            do 
                                                when (not jumped) (put (pos + 1)) -- from Control.Monad
                                                return ""
                                        else
                                            do
                                                let chunkLen = fromIntegral byte
                                                let chunk = BSC.unpack (readAt bs (pos + 1) chunkLen)
                                                rest <- readQNameUtil bs (pos + 1 + chunkLen) jumped
                                                if not (null rest) then return (chunk ++ ['.'] ++ rest)
                                                else return (chunk)
                                        
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

readQName::BS.ByteString -> State Int String
readQName bs = do
                pos <- get
                readQNameUtil bs pos False
                                                

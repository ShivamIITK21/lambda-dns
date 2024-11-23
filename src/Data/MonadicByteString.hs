module Data.MonadicByteString(readWords, read16bit) where

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Control.Monad.State


readWords::BS.ByteString -> Int -> State Int BS.ByteString 
readWords bs n = do
                    offset <- get
                    if offset + 1 >= BS.length bs then error "Reached the end"
                    else do 
                        let readBytes = BS.take n (BS.drop offset bs)
                        put (offset + BS.length readBytes)
                        return readBytes


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




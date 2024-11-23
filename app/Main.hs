module Main (main) where

import qualified Data.MonadicByteString as MS
import qualified Data.ByteString as BS
import qualified Protocol.Types as T
import Control.Monad.State

test::BS.ByteString -> IO()
test bs = do
            let (val, s) = runState (MS.read16bit bs) 0 
            print val
            let (val1, s1) = runState (MS.read16bit bs) s
            print val1
            let (val2, s2) = runState (MS.read16bit bs) s1
            print val2

testParseHeader::BS.ByteString -> IO()
testParseHeader bytes = do
                    let header =  T.parseDNSHeader bytes
                    print header


main :: IO ()
main = do
    let bs1 = BS.pack [0x41, 0xC5, 0x01, 0x20, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    let bs2 = BS.pack [0x41, 0xC5, 0x81, 0x80, 0x00,  0x01,0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
    testParseHeader bs1
    testParseHeader bs2
    

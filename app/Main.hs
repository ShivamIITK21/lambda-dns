module Main (main) where

import qualified Data.MonadicByteString as MS
import qualified Data.ByteString as BS
import qualified Protocol.Types as T
import Control.Monad.State


testParseQuestion:: BS.ByteString -> IO()
testParseQuestion bytes = do
                        let question = evalState (T.parseDNSQuestion bytes) 0
                        print question


main :: IO ()
main = do
    let question = BS.pack [0x06, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x03, 0x63, 0x6f, 0x6d, 0x00, 0x00, 0x01,  0x00, 0x01]
    testParseQuestion question
    

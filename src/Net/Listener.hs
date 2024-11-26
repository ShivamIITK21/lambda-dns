module Net.Listener(startServer) where

import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import qualified Data.ByteString.Char8 as C
import Control.Exception (bracket)
import Protocol.Packet as P
import Net.Stub

resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrSocketType = Datagram }
    head <$> getAddrInfo (Just hints) Nothing (Just port)


open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    return sock

-- Loop to receive and handle UDP packets
listenForDNSReq :: Socket -> IO ()
listenForDNSReq sock = do
    (msg, sender) <- recvFrom sock 512  -- Receive a message up to 1024 bytes
    let maybePacket = P.parseDNSPacket msg
    case maybePacket of
        Nothing -> do
                    print "Could Not parse packet"
                    listenForDNSReq sock
        Just packet -> do
                        print "Parsed! :"
                        print packet
                        res <- sendDNSStubRequest packet
                        case res of
                            Nothing -> do
                                         print "Could not resolve"
                                         listenForDNSReq sock
                            Just resPacket -> do print "Resolved!"
                                                 let response = serialIzeDNSPacket resPacket
                                                 case response of
                                                    Nothing -> do 
                                                                print "Could Not serialized response"
                                                                listenForDNSReq sock
                                                    Just response_s -> do
                                                                        _ <- sendTo sock response_s sender
                                                                        listenForDNSReq sock  -- Keep listening for more packets

startServer:: String -> IO()
startServer s = withSocketsDo $ do 
              addr <- resolve s
              bracket (open addr) close listenForDNSReq
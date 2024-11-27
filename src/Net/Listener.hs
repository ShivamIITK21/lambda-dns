module Net.Listener(startServer) where

import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Net.Resolver as R
import Control.Exception (bracket)
import Protocol.Packet as P
import Protocol.Header as PH
import Protocol.Question as Q 

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
                        let domain = Q.name (head (P.question_list packet))
                        res <- R.lookupDomain domain Q.A R.godIp
                        case res of
                            Nothing -> do
                                         print "Could not resolve"
                                         listenForDNSReq sock
                            Just resPacket -> do print "Resolved!"
                                                 let new_response = serialIzeDNSPacket (replaceId resPacket ((PH.transactionID.header) packet))
                                                 case new_response of
                                                            Nothing -> do listenForDNSReq sock
                                                            Just serial_response -> do
                                                                                     _ <- sendTo sock serial_response sender
                                                                                     listenForDNSReq sock  -- Keep listening for more packets

startServer:: String -> IO()
startServer s = withSocketsDo $ do 
              addr <- resolve s
              bracket (open addr) close listenForDNSReq

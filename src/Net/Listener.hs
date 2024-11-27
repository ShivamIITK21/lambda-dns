module Net.Listener(startServer) where

import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Net.Resolver as R
import Control.Exception (bracket)
import Protocol.Packet as P
import Protocol.Header as PH
import Protocol.Question as Q 
import Net.Stub
import Filterlist(isURLBlocked)
import qualified Data.Set as Set

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
listenForDNSReq :: Set.Set String -> Socket -> IO ()
listenForDNSReq filterlist sock = do
    (msg, sender) <- recvFrom sock 512  -- Receive a message up to 1024 bytes
    let maybePacket = P.parseDNSPacket msg
    case maybePacket of
        Nothing -> do
                    print "Could Not parse packet"
                    listenForDNSReq filterlist sock
        Just packet -> do
                        print "Parsed! :"
-- <<<<<<< Updated upstream
                        let domain = Q.name (head (P.question_list packet))
                        res <- R.lookupDomain domain Q.A R.godIp
                        case res of
                            Nothing -> do
                                         print "Could not resolve"
                                         listenForDNSReq filterlist sock
                            Just resPacket -> do print "Resolved!"
                                                 let new_unserialized = replaceId resPacket ((PH.transactionID.header) packet)
                                                 let new_response = serializeDNSPacket (new_unserialized)
                                                 print "Old packet:"
                                                 print resPacket
                                                 print "New packet:"
                                                 print new_unserialized
                                                 case new_response of
                                                            Nothing -> do listenForDNSReq filterlist sock
                                                            Just serial_response -> do
                                                                                     _ <- sendTo sock serial_response sender
                                                                                     listenForDNSReq filterlist sock  -- Keep listening for more packets
-- =======
--                        print packet
--                        if (foldr (&&) False [(isURLBlocked filterlist name) | (PQ.DNSQuestion name _) <- (question_list packet)])
--                        then do
--                            let maybe_block_resp = serializeDNSPacket (P.DNSPacket {
--                                header = PH.DNSHeader{
--                                    transactionID = ((PH.transactionID.header) packet),
--                                    recursionDesired = True,
--                                    truncatedMessage = False,
--                                    authoritativeAnswer = False,
--                                    opcode = 0,
--                                    response = False,
-- >>>>>>> Stashed changes

--                                    rescode = PH.REFUSED,
--                                    checking = False,
--                                    authed = False,
--                                    z = False,
--                                    recursionAvailable = False,
--
--                                    questions = 1,
--                                    answers = 0,
--                                    authoritativeEntries = 0,
--                                    resourceEntries = 0
--                                },
--                                question_list = question_list packet,
--                                answer_list = [],
--                                authorities_list = [],
--                                resource_list = []
--                            })
--                            case maybe_block_resp of 
--                                Nothing -> do
--                                    print "Failed to send error message"
--                                    listenForDNSReq filterlist sock
--                                Just block_resp -> do
--                                    _ <- sendTo sock block_resp sender
--                                    listenForDNSReq filterlist sock  -- Keep listening for more packets
--                        else do
--                                res <- sendDNSStubRequest packet
--                                case res of
--                                    Nothing -> do
--                                                 print "Could not resolve"
--                                                 listenForDNSReq filterlist sock
--                                    Just resPacket -> do print "Resolved!"
--                                                         let response = serializeDNSPacket resPacket
--                                                         case response of
--                                                            Nothing -> do 
--                                                                        print "Could Not serialized response"
--                                                                        listenForDNSReq filterlist sock
--                                                            Just response_s -> do
--                                                                                let new_response = replaceId resPacket ((PH.transactionID.header) packet)
--                                                                                _ <- sendTo sock response_s sender
--                                                                                listenForDNSReq filterlist sock  -- Keep listening for more packets

startServer:: String -> Set.Set String -> IO()
startServer s filterlist = withSocketsDo $ do 
              addr <- resolve s
              bracket (open addr) close (listenForDNSReq filterlist) 

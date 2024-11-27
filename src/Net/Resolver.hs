module Net.Resolver(lookupQName) where

import Protocol.Question (QueryType, DNSQuestion(..))
import Protocol.Header (defaultHeader)
import Network.Socket
import Protocol.Packet
import Net.IPv4 as IP4
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recvFrom, sendTo)

runUDPClient :: HostName -> ServiceName -> (Socket -> SockAddr -> IO a) -> IO a
runUDPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (openSocket addr) close (\sock -> client sock (addrAddress addr))
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Datagram }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

sendDNSPacket :: DNSPacket -> IPv4 -> IO(Maybe DNSPacket)
sendDNSPacket packet ip = runUDPClient (encodeString ip) "53" $ \sock serverAddr -> do
    -- Send a message to the server
    let maybeSerialnput = serialIzeDNSPacket packet 
    case maybeSerialnput of
        Nothing -> do 
                        return Nothing
        Just serialInput -> do 
                        let message = C.unpack serialInput 
                        _ <- sendTo sock (C.pack message) serverAddr
                        (msg, _) <- recvFrom sock 512 
                        let parseRes = parseDNSPacket msg
                        case parseRes of
                            Nothing -> do 
                                          return Nothing
                            Just parsed -> do
                                            return (Just parsed)
    

lookupQName :: String -> QueryType -> IPv4 -> IO (Maybe DNSPacket)
lookupQName qname_ qtype_ ip = let question = DNSQuestion qname_ qtype_ 
                                   packet = DNSPacket defaultHeader [question] [] [] [] in  
                                   sendDNSPacket packet ip


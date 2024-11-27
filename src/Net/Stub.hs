module Net.Stub(sendDNSStubRequest) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Protocol.Packet
import Control.Monad.State

-- Function to run a UDP client
runUDPClient :: HostName -> ServiceName -> (Socket -> SockAddr -> IO a) -> IO a
runUDPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (openSocket addr) close (\sock -> client sock (addrAddress addr))
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Datagram }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)


sendDNSStubRequest :: DNSPacket -> IO ( Maybe DNSPacket )
sendDNSStubRequest p = runUDPClient "1.1.1.1" "53" $ \sock serverAddr -> do
    -- Send a message to the server
    let res = serializeDNSPacket p

    case res of
        Nothing -> do 
                        print "Could not serialze Packet"
                        return Nothing
        Just pack -> do 
                        let message = C.unpack pack
                        _ <- sendTo sock (C.pack message) serverAddr
                        putStrLn  "Sent: "
                        print pack 
                        (msg, _) <- recvFrom sock 512 
                        print "Received!"
                        let parseRes = parseDNSPacket msg
                        case parseRes of
                            Nothing -> do print "Could Not parse recv packet"
                                          return Nothing
                            Just parsed -> do
                                            print parsed 
                                            return (Just parsed)
    

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


sendDNSStubRequest :: DNSPacket -> IO()
sendDNSStubRequest p = runUDPClient "8.8.8.8" "53" $ \sock serverAddr -> do
    -- Send a message to the server
    let message = C.unpack (serialIzeDNSPacket p)
    sendTo sock (C.pack message) serverAddr
    putStrLn  "Sent: "
    print p

    -- Receive a response from the server
    (msg, _) <- recvFrom sock 1024
    print "Received!"
    let recvP = evalState (parseDNSPacket msg) 0
    print recvP
    

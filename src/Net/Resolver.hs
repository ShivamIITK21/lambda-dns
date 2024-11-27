module Net.Resolver(lookupDomain) where

import Protocol.Question (QueryType(..), DNSQuestion(..))
import Protocol.Header
import Protocol.Record
import Network.Socket
import Protocol.Packet
import Net.IPv4 as IP4
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recvFrom, sendTo)
import Control.Monad(when)

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


godIp :: IP4.IPv4
godIp = fromOctets 198 97 190 53

lookupDomain:: String -> QueryType -> IP4.IPv4 -> IO(Maybe DNSPacket)
lookupDomain domain qtype ip = do
                                    maybe_response <- lookupQName domain qtype ip 
                                    Prelude.print maybe_response
                                    case maybe_response of
                                        Nothing -> return Nothing
                                        Just response -> if (not (null (answer_list response)) && (rescode.header)response == NOERROR) then
                                                            return (Just response)
                                                         else if ((rescode.header) response == NXDOMAIN) then
                                                            return (Just response)
                                                         else do
                                                            maybe_authority_ip <- getAuthorityIPs response godIp 
                                                            case maybe_authority_ip of
                                                                Nothing -> return Nothing
                                                                Just authority_ip -> do
                                                                                        recursvie <- lookupDomain domain qtype authority_ip
                                                                                        return recursvie

                                                                

--     do
--
--         maybe_response <- lookupQName domain qtype ip -- a maybe dnspacket    
--         no_further_action <- (case maybe_response of
--                                 (Just response) -> 
--                                     if (((not.null) (answer_list response) 
--                                             && ((rescode.header) response) == NOERROR)
--                                         || ((rescode.header) response) == NXDOMAIN) 
--                                     then (return True)
--                                     else (return False)
--                                 (Nothing) -> (return True))
--         if no_further_action then
--             do
--                 return maybe_response
--         else do
--                 maybe_ip:: <- (case maybe_response of
--                                 (Just response) -> getAuthorityIPs response ip
--                                 Nothing -> Nothing)::(IO(Maybe (IPv4)))
--                 return (case maybe_ip of 
--                     (Nothing) -> (return Nothing)
--                     (Just new_ip) -> (lookupDomain domain qtype new_ip))
--     
        
            
    



getAuthorityIPs::DNSPacket -> IP4.IPv4 -> IO(Maybe IP4.IPv4)
getAuthorityIPs packet root_ip = 
    let results = [(nsdom, checkNameserver packet nsdom) | R_NS _ nsdom _ <- authorities_list packet]
        ips = [ ip | (_, Just ip) <- results]
        unresolved = [ dom | (dom, Nothing) <- results ]
        lookups = map (\dom -> lookupDomain dom A root_ip) unresolved
        resolutions = map (fmap (\maybe_packet -> 
            do
                packet <- maybe_packet
                let ans_ips = [ans_ip | (R_A _ ans_ip _) <- answer_list packet ] 
                if ((not.null) ans_ips) then return (Prelude.head ans_ips) else Nothing
            )) lookups 
    in  if (not (null ips)) then do return (Just (Prelude.head ips)) else Prelude.head resolutions



-- at this point we have a [String] that we want to convert to IO(Maybe IP4.IPv4)
-- we have lookupQName that will basically do String -> IO(Maybe DNSPacket)
-- so we have [String] -> [IO(Maybe DNSPacket)]
-- now we want to do [IO(Maybe DNSPacket)] -> [IO(Maybe IP4.IPv4)]
-- now we have Maybe (DNSPacket) that in maybe_packet that we want to monadically convert to Maybe(IP4.IPv4), can just use fmap again
-- Maybe (packet) -> (packet -> MaybeIP) -> MaybeIP but middle supplied
-- fmap does Maybe(DNSPacket) -> Maybe(IPv4)

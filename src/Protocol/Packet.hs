module Protocol.Packet(DNSPacket(..), parseDNSPacket, serialIzeDNSPacket, checkNameserver) where

import Data.ByteString as BS
import Control.Monad.State
import Protocol.Header
import Protocol.Question
import Protocol.Record
import Net.IPv4 as IP4
import Debug.Trace(trace)

data DNSPacket = DNSPacket{header::DNSHeader, 
                           question_list::[DNSQuestion], 
                           answer_list::[DNSRecord],
                           authorities_list:: [DNSRecord],
                           resource_list:: [DNSRecord]
                           } deriving(Show, Eq)

parseDNSPacketImpure:: BS.ByteString -> StateT Int Maybe DNSPacket
parseDNSPacketImpure bytes = do
                        _header <- trace "help" (parseDNSHeader bytes)
                        _questions_list <- trace (show _header) (parseDNSQuestionList bytes (questions _header))
--                        let _print2 = trace (show (_questions_list)) 2
                        _answers_list <- trace "help 2" (parseDNSRecordList bytes (answers _header))
                        _authorities_list <- trace "help 3" (parseDNSRecordList bytes (authoritativeEntries _header))
                        _resource_list <- trace "help 4" (parseDNSRecordList bytes (resourceEntries _header))
                        return DNSPacket {header=_header, question_list=_questions_list, answer_list=_answers_list, authorities_list=_authorities_list, resource_list=_resource_list}

parseDNSPacket :: BS.ByteString -> Maybe DNSPacket
parseDNSPacket bytes =  evalStateT (parseDNSPacketImpure bytes) 0


serialIzeDNSPacket :: DNSPacket -> Maybe BS.ByteString
serialIzeDNSPacket packet = do 
                                question_s <- serializeDNSQuestionList (question_list packet)
                                answer_s <- serializeDNSRecordList (answer_list packet)
                                auth_s <- serializeDNSRecordList (authorities_list packet)
                                res_s <- serializeDNSRecordList (resource_list packet)
                                return (BS.concat [header_s, question_s, answer_s, auth_s, res_s])
                            where header_s = serializeDNSHeader (header packet)

checkNameserver::DNSPacket -> String -> Maybe IP4.IPv4
checkNameserver packet nsdom = 
    let ips = [ addr | R_A dom addr _ <- resource_list packet, dom == nsdom]
    in case ips of
        [] -> Nothing
        _ -> Just (Prelude.head ips)
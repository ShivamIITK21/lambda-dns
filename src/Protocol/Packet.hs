module Protocol.Packet(DNSPacket(..), parseDNSPacket, serialIzeDNSPacket, checkNameserver, replaceId) where

import Data.Word
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

replaceId::DNSPacket -> Word16 -> DNSPacket
replaceId resPacket newid = 
        DNSPacket {
            header = DNSHeader {
                transactionID = newid,
                recursionDesired = recursionDesired packetHeader, 
                truncatedMessage = truncatedMessage packetHeader,
                authoritativeAnswer = authoritativeAnswer packetHeader,
                opcode = opcode packetHeader,
                response = response packetHeader,
                rescode = rescode packetHeader,
                checking = checking packetHeader,
                authed = authed packetHeader,
                z = z packetHeader,
                recursionAvailable = recursionAvailable packetHeader,
                questions = questions packetHeader,
                answers = answers packetHeader,
                authoritativeEntries = authoritativeEntries packetHeader,
                resourceEntries = resourceEntries packetHeader},
            question_list = question_list resPacket, 
            answer_list = answer_list resPacket,
            authorities_list = authorities_list resPacket,
            resource_list = resource_list resPacket}
    where packetHeader = header resPacket

parseDNSPacketImpure:: BS.ByteString -> StateT Int Maybe DNSPacket
parseDNSPacketImpure bytes = do
                        _header <- parseDNSHeader bytes
                        _questions_list <- trace (show _header) (parseDNSQuestionList bytes (questions _header))
--                        let _print2 = trace (show (_questions_list)) 2
                        _answers_list <- parseDNSRecordList bytes (answers _header)
                        _authorities_list <- parseDNSRecordList bytes (authoritativeEntries _header)
                        _resource_list <-  parseDNSRecordList bytes (resourceEntries _header)
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

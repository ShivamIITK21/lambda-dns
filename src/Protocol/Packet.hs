module Protocol.Packet(DNSPacket, parseDNSPacket) where

import Data.Word
import Data.Bits
import Data.ByteString as BS
import Data.MonadicByteString as MBS
import Net.IPv4 as IP
import Control.Monad.State
import Protocol.Header
import Protocol.Question
import Protocol.Record

data DNSPacket = DNSPacket{header::DNSHeader, 
                           question_list::[DNSQuestion], 
                           answer_list::[DNSRecord],
                           authorities_list:: [DNSRecord],
                           resource_list:: [DNSRecord]} deriving(Show, Eq)

parseDNSPacket:: BS.ByteString -> State Int DNSPacket
parseDNSPacket bytes = do
                        _header <- parseDNSHeader bytes
                        _questions_list <- parseDNSQuestionList bytes (questions _header)
                        _answers_list <- parseDNSRecordList bytes (answers _header)
                        _authorities_list <- parseDNSRecordList bytes (authoritativeEntries _header)
                        _resource_list <- parseDNSRecordList bytes (resourceEntries _header)
                        return DNSPacket {header=_header, question_list=_questions_list, answer_list=_answers_list, authorities_list=_authorities_list, resource_list=_resource_list}







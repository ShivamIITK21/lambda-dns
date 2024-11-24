module Protocol.Record(DNSRecord, parseDNSRecordList) where

import Protocol.Question
import Data.Word
import Data.ByteString as BS
import Data.MonadicByteString as MBS
import Net.IPv4 as IP
import Control.Monad.State

data DNSRecord = R_UNKNOWN{domain:: String, q_type:: QueryType, data_len:: Word16, ttl:: Word32} | 
                 R_A{domain:: String, addr:: IP.IPv4, ttl:: Word32} deriving(Show, Eq)

wordsToIPv4:: [Word8] -> IP.IPv4
wordsToIPv4 ws = if Prelude.length ws == 4 then IP.fromOctets (ws !! 0) (ws !! 1) (ws !! 2) (ws !! 3)
                 else error "Can only create IPv4 out for 4 words"

parseDNSRecord:: BS.ByteString -> State Int DNSRecord
parseDNSRecord bytes = do
                        _domain <- readQName bytes
                        qtype_num <- read16bit bytes
                        let _qtype = wordToQueryType qtype_num
                        _ <- read16bit bytes
                        _ttl <- read32bit bytes
                        len <- read16bit bytes
                        if _qtype == A then
                            do
                                ip_bytes <- readWords bytes 4
                                let ip = wordsToIPv4 ip_bytes
                                return R_A {domain=_domain, addr=ip, ttl=_ttl}
                        else if _qtype == UNKNOWN then
                            do
                                let len_int = fromIntegral len :: Int
                                _ <- readWords bytes len_int
                                return R_UNKNOWN {domain=_domain, q_type=_qtype, data_len=len, ttl=_ttl}
                        else error "Should not be here"

parseDNSRecordList:: BS.ByteString -> Word16 -> State Int [DNSRecord]
parseDNSRecordList bytes num = do
                                if num == 0 then
                                    do return []
                                else
                                    do
                                        r <- parseDNSRecord bytes
                                        rest <- parseDNSRecordList bytes (num - 1)
                                        return (r:rest)

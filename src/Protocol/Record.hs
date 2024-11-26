module Protocol.Record(DNSRecord, parseDNSRecordList, serializeDNSRecordList) where

import Protocol.Question
import Data.Word
import Data.ByteString as BS
import Data.MonadicByteString as MBS
import Net.IPv4 as IP4
import Net.IPv6 as IP6
import Control.Monad.State
import Data.Helpers 

data DNSRecord = R_A{domain:: String, addr4:: IP4.IPv4, ttl:: Word32} |
                 R_NS{domain::String, nsdname::String, ttl:: Word32} |
                 R_CNAME{domain::String, canonical::String, ttl:: Word32} |
                 R_MX{domain::String, preference::Word16, exchange::String, ttl:: Word32} |
                 R_AAAA{domain:: String, addr6:: IP6.IPv6, ttl:: Word32} |
                 R_UNKNOWN{domain:: String, q_type:: QueryType, data_len:: Word16, ttl:: Word32}
                 deriving(Show, Eq)

wordsToIPv4:: [Word8] -> IP4.IPv4
wordsToIPv4 ws = if Prelude.length ws == 4 then IP4.fromOctets (ws !! 0) (ws !! 1) (ws !! 2) (ws !! 3)
                 else error "Can only create IPv4 out for 4 words"
wordsToIPv6 ws = if Prelude.length ws == 16 then IP6.fromOctets (ws !! 0) (ws !! 1) (ws !! 2) (ws !! 3) (ws !! 4) (ws !! 5) (ws !! 6) (ws !! 7) (ws !! 8) (ws !! 9) (ws !! 10) (ws !! 11) (ws !! 12) (ws !! 13) (ws !! 14) (ws !! 15)
                 else error "Can only create IPv6 out for 16 words"

parseDNSRecord:: BS.ByteString -> State Int DNSRecord
parseDNSRecord bytes = do
                        _domain <- readQName bytes
                        qtype_num <- read16bit bytes
                        let _qtype = wordToQueryType qtype_num
                        _class <- read16bit bytes -- useless, but naming it so it's obvious
                        _ttl <- read32bit bytes
                        len <- read16bit bytes
                        case _qtype of 
                            A -> do
                                ip_bytes <- readWords bytes 4
                                let ip = wordsToIPv4 ip_bytes
                                return R_A {domain=_domain, addr4=ip, ttl=_ttl}
                            AAAA -> do
                                ip_bytes <- readWords bytes 16
                                let ip = wordsToIPv6 ip_bytes
                                return R_AAAA {domain = _domain, addr6 = ip, ttl = _ttl}
                            CNAME -> do
                                _cdomain <- readQName bytes
                                return R_CNAME {domain=_domain, canonical=_cdomain, ttl=_ttl}
                            NS -> do
                                _nsdname <- readQName bytes
                                return R_NS {domain=_domain, nsdname=_nsdname, ttl=_ttl}
                            MX -> do
                                _pref <- read16bit bytes
                                _exch <- readQName bytes
                                return R_MX {domain = _domain, preference = _pref, exchange = _exch, ttl = _ttl}
                            UNKNOWN -> do
                                let len_int = fromIntegral len :: Int
                                _ <- readWords bytes len_int
                                return R_UNKNOWN {domain=_domain, q_type=_qtype, data_len=len, ttl=_ttl}
                            _ -> error "Should not be here"

parseDNSRecordList:: BS.ByteString -> Word16 -> State Int [DNSRecord]
parseDNSRecordList bytes num = do
                                if num == 0 then
                                    do return []
                                else
                                    do
                                        r <- parseDNSRecord bytes
                                        rest <- parseDNSRecordList bytes (num - 1)
                                        return (r:rest)

serializeDNSRecord:: DNSRecord -> BS.ByteString
serializeDNSRecord (R_UNKNOWN {}) = BS.pack [] 

serializeDNSRecord (R_A _dom _addr _ttl) = BS.concat [qname_s, qtype_s, class_s, ttl_s, len_s, oc1, oc2, oc3, oc4]
                                           where qname_s = serializeQName _dom
                                                 qtype_s = word16ToBS (queryTypeWord A)
                                                 class_s = word16ToBS 1
                                                 ttl_s = word32ToBS _ttl
                                                 len_s = word16ToBS 4
                                                 (o1, o2, o3, o4) = toOctets _addr
                                                 oc1 = word8ToBS o1
                                                 oc2 = word8ToBS o2
                                                 oc3 = word8ToBS o3
                                                 oc4 = word8ToBS o4

serializeDNSRecordList:: [DNSRecord] -> BS.ByteString
serializeDNSRecordList xs = BS.concat (Prelude.map serializeDNSRecord xs)

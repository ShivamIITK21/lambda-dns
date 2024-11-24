module Protocol.Types(DNSResCode, parseDNSHeader, parseDNSQuestion, parseDNSRecord, parseDNSPacket) where

import Data.Word
import Data.Bits
import Data.ByteString as BS
import Data.MonadicByteString as MBS
import Net.IPv4 as IP
import Control.Monad.State

data DNSResCode = NOERROR | FORMERR | SERVFAIL | NXDOMAIN | NOTIMP | REFUSED deriving(Eq, Show)

word8ToResCode :: Word8 -> DNSResCode
word8ToResCode 0 = NOERROR
word8ToResCode 1 = FORMERR
word8ToResCode 2 = SERVFAIL
word8ToResCode 3 = NXDOMAIN
word8ToResCode 4 = NOTIMP
word8ToResCode 5 = REFUSED
word8ToResCode _ = error "Invalid Word8 code for DNSResCode"

resCodeValue :: DNSResCode -> Word8
resCodeValue NOERROR = 0
resCodeValue FORMERR = 1
resCodeValue SERVFAIL = 2
resCodeValue NXDOMAIN = 3
resCodeValue NOTIMP = 4
resCodeValue REFUSED = 5


-- | Data type representing a DNS header.
data DNSHeader = DNSHeader
  { 
    transactionID :: Word16,
    recursionDesired:: Bool,
    truncatedMessage:: Bool,
    authoritativeAnswer:: Bool,
    opcode:: Word8,
    response:: Bool,

    rescode:: DNSResCode,
    checking:: Bool,
    authed:: Bool,
    z:: Bool,
    recursionAvailable:: Bool,

    questions:: Word16,
    answers:: Word16,
    authoritativeEntries:: Word16,
    resourceEntries:: Word16
  }
  deriving (Eq, Show)


parseDNSHeader:: ByteString -> State Int DNSHeader 
parseDNSHeader bytes =  do
                            _transactionID <- MBS.read16bit bytes
                            flags <- MBS.read16bit bytes
                            let a = fromIntegral (flags `shiftR` 8) :: Word8
                            let b = fromIntegral (flags .&. 0xFF) :: Word8
                            let _recursionDesired = (a .&. 1) /= 0
                            let _truncatedMessage = (a .&. 2) /= 0
                            let _authoritativeAnswer = (a .&. 4) /= 0
                            let _opcode = (a `shiftR` 3) .&. 0x0F
                            let _response = (a .&. (1 `shiftL` 7)) /= 0

                            let _rescode = word8ToResCode (b .&. 0x0F)
                            let _checking = (b .&. (1 `shiftL` 4)) /= 0
                            let _authed = (b .&. (1 `shiftL` 5)) /= 0
                            let _z = (b .&. (1 `shiftL` 6)) /= 0
                            let _recursionAvailable = (b .&. (1 `shiftL` 7)) /= 0

                            _questions <- MBS.read16bit bytes
                            _answers <- MBS.read16bit bytes
                            _authoritativeEntries <- MBS.read16bit bytes
                            _resourceEntries <- MBS.read16bit bytes

                            return DNSHeader{transactionID =_transactionID,
                                            recursionDesired = _recursionDesired, 
                                            truncatedMessage = _truncatedMessage,
                                            authoritativeAnswer = _authoritativeAnswer,
                                            opcode = _opcode,
                                            response = _response,
                                            rescode = _rescode,
                                            checking = _checking,
                                            authed = _authed,
                                            z = _z,
                                            recursionAvailable = _recursionAvailable,
                                            questions = _questions,
                                            answers = _answers,
                                            authoritativeEntries = _authoritativeEntries,
                                            resourceEntries = _resourceEntries}


-- serializeDNSHeader:: DNSHeader -> BS.ByteString
-- serializeDNSHeader header = 
 

data QueryType = UNKNOWN | A deriving(Eq, Show)

queryTypeWord::QueryType -> Word16
queryTypeWord UNKNOWN = 0
queryTypeWord A = 1 


wordToQueryType :: Word16 -> QueryType
wordToQueryType 0 = UNKNOWN
wordToQueryType 1 = A
wordToQueryType _ = error "Query Type Not Defined" 



data DNSQuestion = DNSQuestion{
    name:: String,
    qtype:: QueryType
} deriving(Eq, Show)

parseDNSQuestion:: BS.ByteString -> State Int DNSQuestion
parseDNSQuestion bytes = do
                            _name <- readQName bytes
                            _qtype <- read16bit bytes
                            _ <- read16bit bytes
                            return DNSQuestion {name=_name, qtype=wordToQueryType _qtype}

parseDNSQuestionList:: BS.ByteString -> Word16 -> State Int [DNSQuestion]
parseDNSQuestionList bytes num = do
                                if num == 0 then
                                    do return []
                                else
                                    do
                                        q <- parseDNSQuestion bytes
                                        rest <- parseDNSQuestionList bytes (num - 1)
                                        return (q:rest)


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







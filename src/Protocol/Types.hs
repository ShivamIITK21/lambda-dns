module Protocol.Types(DNSResCode, parseDNSHeader, parseDNSQuestion) where

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
parseDNSHeader bytes = if BS.length bytes /= 12 then error "DNSHeader Must be 12 bytes long"
                       else  
                       do
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


data DNSRecord = R_UNKNOWN{domain:: String, q_type:: QueryType, data_len:: Word16, ttl:: Word32} | 
                 R_A{domain:: String, addr:: IP.IPv4}



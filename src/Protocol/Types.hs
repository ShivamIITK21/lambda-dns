module Protocol.Types(DNSResCode, parseDNSHeader) where

import Data.Word
import Data.Bits
import Data.ByteString as BS
import Data.MonadicByteString as MBS
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


parseDNSHeaderImpure:: ByteString -> State Int DNSHeader 
parseDNSHeaderImpure bytes = if BS.length bytes /= 12 then error "DNSHeader Must be 12 bytes long"
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

parseDNSHeader:: BS.ByteString -> DNSHeader 
parseDNSHeader bytes = evalState (parseDNSHeaderImpure bytes) 0

-- serializeDNSHeader:: DNSHeader -> BS.ByteString
-- serializeDNSHeader header = 
 

                        
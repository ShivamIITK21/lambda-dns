module Protocol.Question(QueryType(..), DNSQuestion, queryTypeWord,wordToQueryType,parseDNSQuestionList, serializeDNSQuestionList) where

import Data.Word
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Data.MonadicByteString as MBS
import Data.List.Split
import Control.Monad.State
import Data.Helpers

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

                    

serializeDNSQuestion:: DNSQuestion -> BS.ByteString
serializeDNSQuestion question = BS.concat [qname_s, qtype_s, class_s] 
                                where qname_s = serializeQName (name question)
                                      qtype_s = word16ToBS (queryTypeWord (qtype question))
                                      class_s = word16ToBS 1

serializeDNSQuestionList:: [DNSQuestion] -> BS.ByteString
serializeDNSQuestionList xs = BS.concat (Prelude.map serializeDNSQuestion xs)

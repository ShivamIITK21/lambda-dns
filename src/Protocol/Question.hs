module Protocol.Question(QueryType(..), DNSQuestion(..),queryTypeWord,wordToQueryType,parseDNSQuestionList, serializeDNSQuestionList) where

import Data.Word
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Data.MonadicByteString as MBS
import Data.List.Split
import Control.Monad.State
import Data.Helpers
import Debug.Trace(trace)

data QueryType = UNKNOWN | A | AAAA | CNAME | MX | NS deriving(Eq, Show, Enum)

queryTypeWord::QueryType -> Word16
queryTypeWord UNKNOWN   = 0
queryTypeWord A         = 1
queryTypeWord NS        = 2
queryTypeWord CNAME     = 5
queryTypeWord MX        = 15
queryTypeWord AAAA      = 28


wordToQueryType :: Word16 -> QueryType
wordToQueryType 1       = A
wordToQueryType 2       = NS
wordToQueryType 5       = CNAME
wordToQueryType 15      = MX
wordToQueryType 28      = AAAA
wordToQueryType w = trace ("Query Type Not Defined: " ++ show(w)) UNKNOWN



data DNSQuestion = DNSQuestion{
    name:: String,
    qtype:: QueryType
} deriving(Eq, Show)


parseDNSQuestion:: BS.ByteString -> StateT Int Maybe DNSQuestion
parseDNSQuestion bytes = do
                            _name <- readQName bytes
                            _qtype <- read16bit bytes
                            _ <- read16bit bytes
                            return DNSQuestion {name=_name, qtype=(trace (show _qtype) (wordToQueryType _qtype))}

parseDNSQuestionList:: BS.ByteString -> Word16 -> StateT Int Maybe [DNSQuestion]
parseDNSQuestionList bytes num = do
                                if num == 0 then
                                    do return []
                                else
                                    do
                                        q <- parseDNSQuestion bytes
                                        rest <- parseDNSQuestionList bytes (num - 1)
                                        return (q:rest)

                    

serializeDNSQuestion:: DNSQuestion -> Maybe BS.ByteString
serializeDNSQuestion question = do
                                    qname_s <- serializeQName (name question)
                                    return (BS.concat [qname_s, qtype_s, class_s])
                                where   
                                      qtype_s = word16ToBS (queryTypeWord (qtype question))
                                      class_s = word16ToBS 1

serializeDNSQuestionList:: [DNSQuestion] -> Maybe BS.ByteString
serializeDNSQuestionList xs = do 
                                questions <- Prelude.mapM serializeDNSQuestion xs
                                return (BS.concat questions)
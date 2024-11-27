module Filterlist(readFilterList, isURLBlocked) where

import qualified Data.Set as Set
import System.IO

readFilterList:: String -> IO (Set.Set String)
readFilterList filename = do
                            fHandle <- openFile filename ReadMode
                            blocked_urls <- reader fHandle
                            hClose fHandle
                            return (Set.fromList blocked_urls)

isURLBlocked:: Set.Set String -> String -> Bool
isURLBlocked s url = Set.member url s


reader:: Handle -> IO [String]
reader fh = do ineof <- hIsEOF fh
               if ineof 
                then return []
               else do inpStr <- hGetLine fh 
                       rest <- reader fh 
                       return (inpStr : rest)


module Sound.TimeLines.Util where

import Data.List
import Data.Maybe

import Data.Array
import qualified Data.Graph as Graph
import qualified Data.Set as Set

import Prelude
import System.Directory
import Control.Exception
import System.IO.Error 
import System.Directory as DIR
import System.FilePath

import Foreign.Marshal.Array as MA
import Foreign.Ptr
import Foreign.ForeignPtr as FP
import qualified Sound.File.Sndfile as SF
import Prelude as Pr

import Data.Fixed
import qualified Data.Map as Map
import Sound.TimeLines.Types

replaceSessionWindow :: Window -> Session -> Session
replaceSessionWindow newWindow (Session as _ m) = Session as newWindow m


{- SYNTH PATCHES -}
data PatchGraph = PatchGraph {graphVertices::[String],
                              graphEdges::[(String, String)]}

testPatchOrder :: [SynthID] -> [Patch] -> Bool
testPatchOrder order = all (\(s1, s2) ->
                              (findIdx s1 order) < (findIdx s2 order))

findIdx :: Eq a => a -> [a] -> Int
findIdx i is = fromJust $ elemIndex i is

patchNodes :: [Patch] -> [(Graph.Vertex, SynthID)]
patchNodes ps = zip [0..] $ nameSetList ps

nameSetList :: [Patch] -> [SynthID]
nameSetList = Set.toList . Set.fromList . (concatMap tuppleToList)

patchEdges :: [Patch] -> [(Graph.Vertex, [Graph.Vertex])]
patchEdges ps = zip [0..] $ map (findEdges ps) $ nameSetList ps

findEdges :: [Patch] -> SynthID -> [Graph.Vertex]
findEdges [] _ = []
findEdges ps s = [synthIdToVertex ps dest | (src, dest) <- ps, src == s]

synthIdToVertex :: [Patch] -> SynthID -> Graph.Vertex
synthIdToVertex ps id = findIdx id $ nameSetList ps

graphFromPatches :: [Patch] -> Graph.Graph
graphFromPatches ps = array (0, n - 1) $ patchEdges ps
  where n = length $ nameSetList ps
  
sortPatches :: [Patch] -> [SynthID]
sortPatches [] = []
sortPatches ps = [(nameSetList ps)!!i | i <- Graph.topSort $ graphFromPatches ps]

{- /SYNTH PATCHES -}

                 
-- | Returns a list going from start to end in number of steps
fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step  = range / (fromIntegral steps)

-- | Calculates the duration, in seconds, of a TLinfo
ftlDur :: FiniteTimeLine -> Time
ftlDur (FTL _ (s, e)) = e - s

getTimeDomain :: FiniteTimeLine -> [Time]
getTimeDomain ftl@(FTL _ (s, e)) = fromToIn s e $ ftlNumSteps ftl

ftlNumSteps :: FiniteTimeLine -> Int
ftlNumSteps ftl = Pr.floor $ ftlDur ftl * fromIntegral (ftlSR ftl)

-- | Samples the TimeLine according to its info, returns a list of values
getVals :: FiniteTimeLine -> [Value]
getVals ftl@(FTL (_, (sig, _))  _) = map f $ getTimeDomain ftl
  where f = runSig sig
        
--user ehird, https://stackoverflow.com/questions/8502201/remove-file-if-it-exists#8502391
-- | Deletes a file if it already exists
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | Returns the temp directory on the current OS
getTLTempDir :: IO FilePath
getTLTempDir = do
  osTemp <- DIR.getTemporaryDirectory
  let tlTemp = addTrailingPathSeparator $ joinPath [osTemp, "TimeLines", "buffers"]
  DIR.createDirectoryIfMissing True tlTemp
  return tlTemp

-- | Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: FilePath -> FiniteTimeLine -> IO SF.Handle
openHandle path ftl = SF.openFile path SF.ReadWriteMode info
        where format = SF.Format SF.HeaderFormatW64 SF.SampleFormatDouble SF.EndianFile
              info = SF.Info (ftlNumSteps ftl) (ftlSR ftl) 1 format 1 True

-- | Samples a constant sig for t = 0
constSigToValue :: Signal a -> a
constSigToValue sig = runSig sig 0

-- | Closes a handle
closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose

-- | Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: FiniteTimeLine -> IO (Ptr Value)
getArrayPtr ftl = MA.newArray $ getVals ftl


{- GENERAL PURPOSE -}
tuppleToList :: (a, a) -> [a]
tuppleToList (x, y) = [x, y]

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

import Data.Fixed
import qualified Data.Map as Map
import Sound.TimeLines.Types

removeDups :: Ord a => [a] -> [a]
removeDups = Set.toList . Set.fromList

{- SYNTH PATCHES -}

data PatchGraph = PatchGraph { graphVertices::[String]
                             , graphEdges::[(String, String)]
                             }

findIdx :: Eq a => a -> [a] -> Int
findIdx i is = fromJust $ elemIndex i is

patchNodes :: [Patch] -> [(Graph.Vertex, SynthID)]
patchNodes ps = zip [0..] $ nameSetList ps

nameSetList :: [Patch] -> [SynthID]
nameSetList = removeDups . concatMap tuppleToList

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

flattenPatches :: [Patch] -> [SynthID]
flattenPatches = concatMap tuppleToList

{- /SYNTH PATCHES -}

         
-- | Returns a list going from start to end in number of steps
fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step  = range / (fromIntegral steps)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | Returns the temp directory of the current OS
getTLTempDir :: IO FilePath
getTLTempDir = do
  osTemp <- DIR.getTemporaryDirectory
  let tlTemp = addTrailingPathSeparator $ joinPath [osTemp, "TimeLines", "buffers"]
  DIR.createDirectoryIfMissing True tlTemp
  return tlTemp


-- | Samples a constant sig for t = 0
constSigToValue :: Signal a -> a
constSigToValue sig = sampleSig sig 0

{- GENERAL PURPOSE -}
tuppleToList :: (a, a) -> [a]
tuppleToList (x, y) = [x, y]

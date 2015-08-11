-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.GenBounds
-- Copyright   :  (c) Doug Beardsley 2015
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal gen-bounds command
-----------------------------------------------------------------------------
module Distribution.Client.GenBounds (
    genBounds
  ) where

import Data.Version
         ( Version(..) )
import Distribution.Package
         ( Dependency(..), PackageName(..) )
import Distribution.PackageDescription
         ( buildDepends, packageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Compiler
         ( CompilerInfo )
import Distribution.Simple.Utils
         ( tryFindPackageDesc )
import Distribution.System
         ( Platform )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Version
         ( VersionRange(..), foldVersionRange )
import System.Directory
         ( getCurrentDirectory )

incVersion :: Version -> Version
incVersion (Version vs ts) = Version (go vs) ts
  where
    go [] = []
    go [x] = [x+1]
    go (x:xs) = x : go xs

data VersionBound = ClosedBound Version
                  | OpenBound Version
  deriving (Show)

instance Eq VersionBound where
    ClosedBound a == ClosedBound b = a == b
    ClosedBound _ == OpenBound _ = False
    OpenBound _ == ClosedBound _ = False
    OpenBound a == OpenBound b = a == b

instance Ord VersionBound where
    compare (ClosedBound b1) (ClosedBound b2) = compare b1 b2
    compare (ClosedBound b1) (OpenBound b2) =
        case compare b1 b2 of
          EQ -> GT
          o -> o
    compare (OpenBound b1) (ClosedBound b2) =
        case compare b1 b2 of
          EQ -> LT
          o -> o
    compare (OpenBound b1) (OpenBound b2) = compare b1 b2

--lb (VersionIntervals []) = Nothing
--lb (VersionIntervals (x:xs)) = go x `mplus` lb xs
--  where
--    go (LowerBound v _, _) = Just v
--
--
--ub (VersionIntervals []) = Nothing
--ub (VersionIntervals (x:xs)) = go x `mplus` ub xs
--  where
--    go (_, NoUpperBound) = Nothing
--    go (_, UpperBound v) = Just v


--lowerBound =
--    foldVersionRange Nothing
--                     (Just . ClosedBound)
--                     (Just . OpenBound)
--                     Nothing
--                     unionLower
--                     intersectLower

unionLower a b =
    case (a,b) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just u1, Just u2) -> Just $ min u1 u2

intersectLower a b =
    case (a, b) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just u1) -> Just u1
      (Just u1, Nothing) -> Just u1
      (Just u1, Just u2) -> Just $ max u1 u2


--upperBoundNew =
--    foldVersionRange Nothing
--                     (Just . ClosedBound)
--                     Nothing
--                     (Just . OpenBound)
--                     unionUpper
--                     intersectUpper

unionUpper a b =
    case (a,b) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just u1, Just u2) -> Just $ max u1 u2

intersectUpper a b =
    case (a, b) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just u1) -> Just u1
      (Just u1, Nothing) -> Just u1
      (Just u1, Just u2) -> Just $ min u1 u2


upperBound :: VersionRange -> Maybe VersionBound
upperBound AnyVersion = Nothing
upperBound (ThisVersion v) = Just $ ClosedBound v
upperBound (LaterVersion v) = Nothing
upperBound (EarlierVersion v) = Just $ OpenBound v
upperBound (WildcardVersion v) = Just $ OpenBound $ incVersion v
upperBound (UnionVersionRanges r1 r2) = 
    case (upperBound r1, upperBound r2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just u1, Just u2) -> Just $ max u1 u2
upperBound (IntersectVersionRanges r1 r2) = 
    case (upperBound r1, upperBound r2) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just u1) -> Just u1
      (Just u1, Nothing) -> Just u1
      (Just u1, Just u2) -> Just $ min u1 u2
upperBound (VersionRangeParens r) = upperBound r

genBounds
    :: Verbosity
    -> Platform
    -> CompilerInfo
    -> IO ()
genBounds verbosity platform cinfo = do
    putStrLn "genBounds isn't very complicated yet"
    putStrLn $ "verbosity: " ++ show verbosity
    cwd <- getCurrentDirectory
    path <- tryFindPackageDesc cwd
    gpd <- readPackageDescription verbosity path
    let epd = finalizePackageDescription [] (const True) platform cinfo [] gpd
    case epd of
      Left _ -> putStrLn "finalizePackageDescription failed"
      Right (pd,_) -> do
        let deps = buildDepends pd
        mapM_ (putStrLn . showDep) deps
    putStrLn "done."

showDep dep = unlines
    [ depName dep ++ " " ++ show (depUpper dep)
    --, show $ depVersion dep
    ]


depName (Dependency (PackageName nm) _) = nm
depVersion (Dependency _ vr) = vr
depUpper (Dependency _ vr) = upperBound vr


{-|
Module      : Route
Description : Data types and parser for stops and lines
Copyright   : (c) Anton Ekblad, Alex Gerdes, 2019
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
Portability :  -
-}

module Route
  ( Stop(..)
  , LineTable(..)
  , LineStop(..)
  , readStops
  , readLines
  ) where

import Control.Applicative
import Control.Monad
import Data.List

-- | Description of a stop.
data Stop = Stop
  { name     :: String              -- ^ The stop's name.
  , position :: (Integer, Integer)  -- ^ Coordinates (x, y).
                                    --
                                    -- Invariant: The coordinates range from 0 to 1000.
  } deriving Show

-- | Description of a line.
data LineTable = LineTable
  { lineNumber :: Integer     -- ^ The line number.
  , stops      :: [LineStop]  -- ^ Stops (in the order in which they are visited).
                              --
                              -- Invariant: The list contains at least one element.
  } deriving Show

-- | Description of a stop, as part of a line.
data LineStop = LineStop
  { stopName :: String   -- ^ The stop's name.
  , time     :: Integer  -- ^ The travel time (in minutes) from the previous stop.
                         --
                         -- Invariant: A non-negative number, 0 if this is the first stop.
  } deriving Show

-- | Tries to parse a non-negative integer.
readNat :: String -> Maybe Integer
readNat s = case filter (null . snd) $ reads s of
  [(x, _)] | x >= 0 -> Just x
  _                 -> Nothing

-- | @split n xs@ splits up @xs@ into groups of size @n@ (and possibly a final,
split :: Integer -> [a] -> [[a]]
split n [] = []
split n xs = ys : split n zs
 where 
  (ys, zs) = genericSplitAt n xs

-- | Tries to parse a stop. If an error is encountered, then an error message is
-- returned (@'Left' msg@).
readStop :: [String] -> Either String Stop
readStop ss = case ss of
  name : coordinates@[_, _] ->
    case mapM readNat coordinates of
      Just cs@[x, y] | all inRange cs ->
        Right (Stop { name = name, position = (x, y) })
      _ -> err
  _ -> err
 where
  inRange c = 0 <= c && c <= 1000
  err       = Left $ "Incorrectly formatted stop:\n" ++ unwords ss

-- | Tries to read a list of stops from the given file. If an error is
-- encountered, then an error message is returned (@'Left' msg@).
readStops :: FilePath -> IO (Either String [Stop])
readStops f = mapM readStop . split 3 . words <$> readFile f

-- | Tries to parse a \"line stop\" (not the first one for a line, i.e., the
-- 'time' must be given). If an error is encountered, then an error message is
-- returned (@'Left' msg@).
readLineStop :: [String] -> Either String LineStop
readLineStop ss = case ss of
  [name, time] -> case readNat time of
    Just time -> Right (LineStop { stopName = name, time = time })
    _         -> err
  _ -> err
 where
  err = Left $ "Incorrectly formatted line stop:\n" ++ unwords ss

-- | Tries to read information about lines from the given file. If an error is
-- encountered, then an error message is returned (@'Left' msg@).
readLines :: FilePath -> IO (Either String [LineTable])
readLines f = parse . words <$> readFile f
  where
  parse []           = return []
  parse (l : n : ss) =
    case mapM readNat [l, n] of
      Just [line, noStops]
        | noStops <= 0 -> Left $ "Empty lines not allowed:\n" ++
                                 unwords [l, n]
        | otherwise    -> do
          let noTokens      = 2 * noStops - 1
              (stops, rest) = genericSplitAt noTokens ss
          when (genericLength stops < noTokens) $ do
            Left $ "Incorrectly formatted line:\n" ++
                   unwords (l : n : stops)
          case stops of
            stop : stops -> do
              stop  <- return (LineStop { stopName = stop, time = 0 })
              stops <- mapM readLineStop (split 2 stops)
              rest  <- parse rest
              return (LineTable { lineNumber = line
                                , stops      = stop : stops
                                } : rest)
      _ -> Left $ "Incorrectly formatted line header:\n" ++
                  unwords [l, n]

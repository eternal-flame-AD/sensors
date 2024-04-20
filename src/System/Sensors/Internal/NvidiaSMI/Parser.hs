{-# LANGUAGE ScopedTypeVariables #-}

module System.Sensors.Internal.NvidiaSMI.Parser (readNvidiaCsvLine, parseHelpQueryGpu, parseGPUUuids) where

import Control.Exception (catch)
import GHC.IO.Handle
import Text.ParserCombinators.ReadP

-- | Read a line from nvidia-smi --query-gpu
-- Nvidia adds a whitespace at the beginning of the line, so we need to trim it
readNvidiaCsvLine :: ReadP [String]
readNvidiaCsvLine = sepBy (many (satisfy (/= ','))) (string ", ")

readPropertyQuotes :: ReadP String
readPropertyQuotes = do
  _ <- char '"'
  s <- many1 (satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ', '-', '_', '.']))
  _ <- char '"'
  return s

readFullPropertyHelp :: ReadP String
readFullPropertyHelp = do
  res <- sepBy readPropertyQuotes (string " or ")
  case res of
    [] -> pfail
    x : _ -> return x

readGPUUuid :: ReadP String
readGPUUuid = do
  _ <- manyTill get (string "(UUID: ")
  uuid <- many1 (satisfy (/= ')'))
  _ <- char ')'
  return uuid

-- | Parse the output of nvidia-smi --help-query-gpu
--    Input is double-quoted strings optionally separated by " or "
--    Ignores invalid lines
parseHelpQueryGpu :: Handle -> IO [String]
parseHelpQueryGpu h = go []
  where
    go :: [String] -> IO [String]
    go acc = do
      res <- (Left <$> hGetLine h) `catch` \(_ :: IOError) -> return $ Right acc
      case res of
        Right x -> return x
        Left line -> case readP_to_S readFullPropertyHelp line of
          [(x, "")] -> go (x : acc)
          _ -> go acc

-- | Parse the output of nvidia-smi --list-gpus
--    Input is a line with a GPU UUID
parseGPUUuids :: Handle -> IO [String]
parseGPUUuids h = go []
  where
    go :: [String] -> IO [String]
    go acc = do
      res <- (Left <$> hGetLine h) `catch` \(_ :: IOError) -> return $ Right acc
      case res of
        Right x -> return x
        Left line -> case readP_to_S readGPUUuid line of
          [(x, "")] -> go (x : acc)
          _ -> go acc

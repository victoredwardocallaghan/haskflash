{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : GPL2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module contains the main program.
-}

module Main where

import LibFtdi

import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)
import Control.Monad (when)
import           Data.Bits
import qualified Data.ByteString as BS
import Foreign (allocaBytes)
import Options.Applicative
import System.IO

import Cmd
import Ftdi
import FlashCmd

-- | ..
data ProgramOptions  = ProgramOptions { device  :: Maybe String
                                      , entire  :: Bool
                                      , first   :: Bool
                                      , verify  :: Bool
                                      , bulk    :: Bool
                                      , erase   :: Bool
                                      , sram    :: Bool
                                      , verbose :: Bool
                                      , filep   :: String
                                      }

-- | ..
parseOpts :: Parser ProgramOptions
parseOpts = ProgramOptions
  <$> (optional . strOption)
      ( long "device"
      <> short 'd'
      <> metavar "\"i:vid:pid\""
      <> help "Use the specified USB device:" )
  <*> switch
      ( long "entire"
      <> short 'r'
      <> help "Read entire flash (32Mb / 4MB) and write to file" )
  <*> switch
      ( long "first"
      <> short 'R'
      <> help "Read first 256 kB from flash and write to file" )
  <*> switch
      ( long "verify"
      <> short 'c'
      <> help "Do not write flash, only verify (check)" )
  <*> switch
      ( long "bulk"
      <> short 'b'
      <> help "Bulk erase entire flash before writing" )
  <*> switch
      ( long "erase"
      <> short 'n'
      <> help "Do not erase flash before writing" )
  <*> switch
      ( long "sram"
      <> short 'S'
      <> help "Perform SRAM programming" )
  <*> switch
      ( long "verbose"
      <> short 'v'
      <> help "Verbose output" )
  <*> strArgument
      ( help "<filename>" )
     
-- | ..
entry :: ProgramOptions -> IO ()
entry (ProgramOptions h False False False False False True ver fp) = do
  dev <- initFTDI h
  --
  -- Reset
  putStrLn "reset.."
  setGPIO dev (False, False)
  threadDelay 100
  setGPIO dev (False, True)
  threadDelay 2000

  --
  -- Program
  withBinaryFile fp ReadMode $ \hdl -> do putStrLn "programming.."
                                          fix $ \loop -> do
                                                 buf <- BS.hGet hdl 4096
                                                 when ((BS.length buf) > 0) $ do
                                                   sendSPI dev (BS.unpack buf)
                                                   when ver $ putStrLn ("sending " ++ show (BS.length buf) ++ " bytes.")
                                                   putChar '.'
                                                   loop

  -- add 48 dummy bits
  sendByte dev 0x8F
  sendByte dev 0x05
  sendByte dev 0x00

  -- add 1 more dummy bit
  sendByte dev 0x8E
  sendByte dev 0x00

  cleanupFTDI dev
  -- .

entry (ProgramOptions h em rm vm be er False ver fp) = do
  dev <- initFTDI h
  --
  -- Reset
  putStrLn "reset.."
  setGPIO dev (True, False)
  threadDelay 250000
  cd <- getCDone dev
  putStrLn $ "cdone: " ++ cd
  flashReadID dev

  --
  -- Program
  when (not (em .|. rm .|. vm)) $ do putStrLn "programming.."
                                  -- open file in read mode
                                     when (not er) $ do
                                       if be then do
                                        flashWriteEnable dev ver
                                        flashBulkErase dev
                                        flashWait dev ver
                                       else undefined

  --
  -- Read/Verify
  if (em .|. rm) then readFlashToFile dev fp (4 * 1024 * 1024) ver
  else verifyFlashInFile dev fp ver

  --
  -- Reset
  setGPIO dev (True, True)
  threadDelay 250000
  cd <- getCDone dev
  putStrLn $ "cdone: " ++ cd

  cleanupFTDI dev
  -- .

entry _ = return ()

-- ..
readFlashToFile :: DeviceHandle -> FilePath -> Int -> Bool -> IO ()
readFlashToFile dev fp sz ver = withBinaryFile fp AppendMode $ \hdl -> do
  putStrLn "reading.."
  loop hdl dev 0 ver
  where loop h d a v = when (a < sz) $ do
          buf <- flashRead d a 256 v
          BS.hPut h buf
          loop h d (a+256) v

-- ..
verifyFlashInFile :: DeviceHandle -> FilePath -> Bool -> IO ()
verifyFlashInFile dev fp ver = withBinaryFile fp ReadMode  $ \hdl -> do
  putStrLn "reading.. (FIXME)"
  loop hdl dev 0 ver
  where loop h d a v = do
          eof <- hIsEOF h
          when (not eof) $ do
            fbuf <- BS.hGet h 256
            sbuf <- flashRead dev a 256 ver
            if fbuf /= sbuf then
              putStrLn "Found difference between flash and file!"
            else putStrLn "VERIFY OK"
            loop h d (a+256) v

-- | ..
main :: IO ()
main = execParser opts >>= entry
  where opts = info (helper <*> parseOpts)
          ( fullDesc
         <> progDesc "Flash programming tool for FTDI-based programmers"
         <> header "haskflash -- programming tool for FTDI-based programmers"
         <> footer "Notes: An unmodified iCEstick can only be programmed via the serial flash." )

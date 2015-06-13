{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : GPL2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module contains ..
-}

module FlashCmd ( flashRead
                , flashWait
                , flashWriteEnable
                , flashBulkErase
                , flashSectorErase
                , flashReadID
                , flashProgram
		) where

import LibFtdi

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Word

import Cmd
import Misc

-- | ..
flashRead :: DeviceHandle
          -> Int -- ^ address
          -> Int -- ^ size
          -> Bool -- ^ verbose
          -> IO BS.ByteString
flashRead dev addr sz ver = do
  when ver $ putStrLn $ "read " ++ show addr ++ " +" ++ show sz
  setGPIO dev (False, False)
  sendSPI dev (buildCMD 0x03 addr)
  d <- xferSPI dev (replicate sz 0)
  setGPIO dev (True, False)
  when ver $ putStrLn (bsToHexString d)
  return d

-- | ..
flashWait :: DeviceHandle -> Bool -> IO ()
flashWait dev ver = do
  putStrLn "waiting.."
  -- loop begin
  let d = [0x05]
  setGPIO dev (False, False)
  xferSPI dev d
  setGPIO dev (True, False)
  -- if ((data[1] & 0x01) == 0) break
  when ver $ putChar '.'
  threadDelay 250000
  -- loop end??

-- | ..
flashWriteEnable :: DeviceHandle -> Bool -> IO ()
flashWriteEnable dev ver = do
  when ver $ putStrLn "write enable.."
  let d = [0x06]
  setGPIO dev (False, False)
  xferSPI dev d
  setGPIO dev (True, False)

-- | ..
flashBulkErase :: DeviceHandle -> IO ()
flashBulkErase dev = do
  putStrLn "bulk erase.."
  let d = [0xC7]
  setGPIO dev (False, False)
  xferSPI dev d
  setGPIO dev (True, False)

-- | ..
flashSectorErase :: DeviceHandle -> Int -> IO ()
flashSectorErase dev addr = do
  putStrLn $ "sector erase " ++ show addr
  setGPIO dev (False, False)
  sendSPI dev (buildCMD 0xD8 addr)
  setGPIO dev (True, False)

-- | ..
flashReadID :: DeviceHandle -> IO ()
flashReadID dev = do
  setGPIO dev (False, False)
  d <- xferSPI dev (replicate 21 0x9E)
  setGPIO dev (True, False)
  putStrLn $ "flash ID: " ++ bsToHexString d

-- | ..
flashProgram :: DeviceHandle
             -> Bool          -- ^ Verbosity
             -> Int           -- ^ Address
	     -> BS.ByteString -- ^ Data
             -> IO ()
flashProgram dev v addr d = do
  when v $ putStrLn $ "program " ++ show addr
  setGPIO dev (False, False)
  sendSPI dev (buildCMD 0x02 addr)
  sendSPI dev (BS.unpack d)
  setGPIO dev (True, False)

buildCMD :: Int -> Int -> [Word8]
buildCMD cmd addr = fmap fromIntegral ca
  where ca = [cmd, (shiftR addr 16), (shiftR addr 8), addr]

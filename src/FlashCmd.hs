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

import Cmd


-- | ..
flashRead :: DeviceHandle
          -> Int -- ^ address
          -> Int -- ^ size
          -> Bool -- ^ verbose
          -> IO () -- BS.ByteString
flashRead dev addr sz ver = do
  when ver $ putStrLn $ "read " ++ show addr ++ " +" ++ show sz
  let cmd = [0x03, (shiftR addr 16), (shiftR addr 8), (fromIntegral addr)]
  setGPIO dev (False, False)
  sendSPI dev (fmap fromIntegral cmd)
--  d <- xferSPI dev sz
  setGPIO dev (True, False)
--  return d

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
  let cmd = [0xD8, (shiftR addr 16), (shiftR addr 8), addr]
  setGPIO dev (False, False)
  sendSPI dev (fmap fromIntegral cmd)
  setGPIO dev (True, False)

-- | ..
flashReadID :: DeviceHandle -> IO ()
flashReadID dev = do
  let d = [0x9E]
  setGPIO dev (False, False)
  xferSPI dev d
  setGPIO dev (True, False)
  putStrLn $ "flash ID: " ++ show d

-- | ..
flashProgram :: DeviceHandle
             -> Bool          -- ^ Verbosity
             -> Int           -- ^ N
             -> Int           -- ^ Address
	     -> BS.ByteString -- ^ Data
             -> IO ()
flashProgram dev v n addr d = undefined

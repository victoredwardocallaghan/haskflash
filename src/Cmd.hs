{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : GPL2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module contains ..
-}

module Cmd ( setGPIO
           , recvByte
           , sendByte
           , sendSPI
           , xferSPI
	   ) where

import LibFtdi

import           Control.Concurrent (threadDelay)
import           Control.Monad (when, replicateM)
import           Control.Monad.Fix (fix)
import           Data.Word
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Maybe (listToMaybe)

import Misc

-- ?
setGPIO :: DeviceHandle -> (Bool, Bool) -> IO ()
setGPIO dev (slaveset, creset) = do
  sendByte dev 0x80
  sendByte dev $ setFlags 1 [ (slaveset, 0x10) -- ADBUS4 (GPIOL0)
                            , (creset,   0x80) -- ADBUS7 (GPIOL3)
                            ]
  sendByte dev 0x93
  where setFlags :: Word8 -> [(Bool, Word8)] -> Word8
        setFlags = foldCond (.|.)
        --
        foldCond :: (a -> a -> a) -> a -> [(Bool, a)] -> a
        foldCond f = foldr (\(b, x) acc -> if b then f acc x else acc)

-- ?
recvByte :: DeviceHandle -> IO Word8
recvByte dev = fix $ \loop -> do
  d <- ftdiReadData dev 1
  case d of
    Nothing -> do threadDelay 100
                  loop
    Just x  -> do let y = listToMaybe $ BS.unpack x
                  case y of
                   Nothing -> undefined -- XXX should be unreachable
                   Just z  -> return z

-- ?
sendByte :: DeviceHandle -> Word8 -> IO ()
sendByte dev w = do
  r <- ftdiWriteData dev (BS.pack [w])
  when (r /= 0) $ do putStrLn $ "Write error (single byte, rc=" ++ show r ++ ", expected 1)"
                     checkAndcleanup $ Just dev
  return () -- XXX

-- ?
sendSPI :: DeviceHandle -> [Word8] -> IO ()
sendSPI dev d = do
  let sz = length d
  sendByte dev 0x11
  sendByte dev $ fromIntegral (sz - 1)
  sendByte dev $ fromIntegral (shiftR (sz - 1) 8)
  rc <- ftdiWriteData dev (BS.pack d)
  when (rc /= sz) $ do putStrLn $ "Write error (chunk, rc=" ++ show rc ++ ", expected " ++ show sz ++ ")."
                       checkAndcleanup $ Just dev

-- ?
xferSPI :: DeviceHandle -> [Word8] -> IO BS.ByteString
xferSPI dev w = do
  let sz = length w
  let d  = BS.pack w
--  when (sz < 1) $ return () -- XXX
  sendByte dev 0x31
  sendByte dev (fromIntegral (sz - 1))
  sendByte dev (fromIntegral (shiftL (sz - 1) 8))
  r <- ftdiWriteData dev d
  when (r /= sz) $ do putStrLn $ "Write error (chunk, rc=" ++ show r ++ ", expected " ++ show sz ++ ")."
                      checkAndcleanup $ Just dev
  ba <- replicateM sz $ recvByte dev
  return $ BS.pack ba

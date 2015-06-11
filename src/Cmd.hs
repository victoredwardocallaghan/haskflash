{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : GPL2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module contains ..
-}

module Cmd where

import LibFtdi

import           Control.Monad (when, replicateM)
import           Data.Word
import           Data.Bits
import qualified Data.ByteString as BS


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
recvByte = undefined
--recvByte dev = fix $ \loop -> do
--  (rc, d) <- ftdiReadData dev 1
--  when (rc /= 1) $ do
--      threadDelay 100
--      loop

-- ?
sendByte :: DeviceHandle -> Word8 -> IO ()
sendByte dev w = do
  r <- ftdiWriteData dev (BS.pack [w])
  when (r /= 0) $ putStrLn $ "Write error (single byte, rc=" ++ show r ++ ", expected 1)"
  return () -- XXX

-- ?
sendSPI :: DeviceHandle -> [Word8] -> IO ()
sendSPI dev d = do
  let sz = length d
  sendByte dev 0x11
  sendByte dev $ fromIntegral (sz - 1)
  sendByte dev $ fromIntegral (shiftR (sz - 1) 8)
  rc <- ftdiWriteData dev (BS.pack d)
  when (rc /= sz) $ putStrLn $ "Write error (chunk, rc=" ++ show rc ++ ", expected " ++ show sz ++ ")."

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
  when (r /= sz) $ putStrLn $ "Write error (chunk, rc=" ++ show r ++ ", expected " ++ show sz ++ ")."
  ba <- replicateM sz $ recvByte dev
  return $ BS.pack ba


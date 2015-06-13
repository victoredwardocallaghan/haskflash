{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : GPL2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module contains various misc actions,
  such as for example, generic error handling
  and cleanups.
-}

module Misc ( checkAndcleanup
            , bsToHexString
            ) where

import qualified Data.ByteString as BS
import           Data.List.Split (chunksOf)
import           System.Exit (exitFailure)
import           Text.Printf (printf)

import LibFtdi

-- | Check that we are able to receive bytes
--   and if things are bad clean up and exit.
checkAndcleanup :: Maybe DeviceHandle -> IO ()
checkAndcleanup dev = do
  putStrLn "ABORT"
  case dev of
   Nothing   -> putStrLn "No device connected"
   Just dev' -> do checkRx dev'
                   ftdiUSBClose dev'
                   ftdiDeInit dev'
  exitFailure
  
checkRx :: DeviceHandle -> IO ()
checkRx  = loop
    where loop dev = do
           d <- ftdiReadData dev 1
           case d of
            Nothing -> do putStrLn $ "unexpected rx byte: " ++ show d
                          loop dev
            Just _  -> return ()

-- | Given a ByteString return a String of bytes in the hex representation
bsToHexString :: BS.ByteString -> String
bsToHexString x = unlines $ fmap (concatMap (printf "0x%.2X ")) ls
  where ls = chunksOf 32 (BS.unpack x)

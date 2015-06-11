{-|
  Module      : $Header$
  Copyright   : (c) 2015 Edward O'Callaghan
  License     : GPL2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module contains ..
-}

module HaskFlash.Ftdi where

import LibFtdi

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)

import HaskFlash.Cmd

initFTDI' :: IO DeviceHandle
initFTDI' = do
  putStrLn "init.."
  dev <- ftdiInit
  case dev of
    Left e      -> throwIO e
    Right ftdic -> return ftdic

-- | Initialize USB connection to FT2232H
initFTDI :: IO DeviceHandle
initFTDI = do
  ftdic <- initFTDI'
  ftdiSetInterface ftdic INTERFACE_ANY

--	if (devstr != NULL) {
--		if (ftdi_usb_open_string(&ftdic, devstr)) {
--			printf("Can't find iCE FTDI USB device (device string %s).\n", devstr);

  ftdiUSBOpen ftdic (0x0403, 0x6010)
  -- "Can't find iCE FTDI USB device (vedor_id 0x0403, device_id 0x6010)."

  ftdiUSBReset ftdic
  -- "Failed to reset iCE FTDI USB device."

  usbPurgeBuffers ftdic
  -- "Failed to purge buffers on iCE FTDI USB device."

  ftdiSetBitMode ftdic 0xFF BITMODE_MPSSE
  -- "Failed set BITMODE_MPSSE on iCE FTDI USB device."

  -- enable clock divide by 5
  sendByte ftdic 0x8B

  -- set 6 MHz clock
  sendByte ftdic 0x86
  sendByte ftdic 0x00
  sendByte ftdic 0x00

  -- putStrLn $ "cdone: %s\n", get_cdone() ? "high" : "low"

  setGPIO ftdic (True, True)
  threadDelay 100000

  return ftdic

  
-- | Exit
cleanupFTDI :: DeviceHandle -> IO ()
cleanupFTDI ftdic = do
  putStrLn "Bye."
  ftdiDisableBitBang ftdic
  ftdiUSBClose ftdic
  ftdiDeInit ftdic
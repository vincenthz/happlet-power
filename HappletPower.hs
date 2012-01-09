{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Main
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : linux
--
module Main where

import Graphics.UI.Gtk
import System.Directory

import Data.IORef
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad
import Text.Printf
import Prelude hiding (catch)

checkInterval      = 10 -- seconds

thresholdWarning   = 25 -- percents
thresholdLow       = 10 -- percents
thresholdEmergency = 3  -- percents

data BatStatus = Normal | Warning | Low | Emergency
	deriving (Show,Eq)
data Status = OnMain | OnBattery BatStatus
	deriving (Show,Eq)

powerSupplyRootPath = "/sys/class/power_supply/"
powerSupplyPath n = powerSupplyRootPath ++ n ++ "/"
powerSupplyProperty n prop = powerSupplyPath n ++ prop

enumPowerSupplies = filter (not . isDotFile) `fmap` getDirectoryContents powerSupplyRootPath
	where
		isDotFile ('.':_) = True
		isDotFile _       = False

enumBatteries = enumPowerSupplies >>= filterM isBattery
	where isBattery n = ("Battery" ==) `fmap` readType n

enumMains = enumPowerSupplies >>= filterM isBattery
	where isBattery n = ("Mains" ==) `fmap` readType n

readSysFile path = firstLine `fmap` readFile path
	where firstLine = head . lines

readType :: String -> IO String
readType n = readSysFile (powerSupplyProperty n "type")

readStatus :: String -> IO String
readStatus n = readSysFile (powerSupplyProperty n "status")

readChargeFull, readChargeNow :: String -> IO Int
readChargeFull n = read `fmap` readSysFile (powerSupplyProperty n "charge_full_design")
readChargeNow n = read `fmap` readSysFile (powerSupplyProperty n "charge_now")

isOnline :: String -> IO Bool
isOnline n = catch (((== 1) . readInt) `fmap` readSysFile (powerSupplyProperty n "online")) (\(_ :: SomeException) -> return False)
	where
		readInt :: String -> Int
		readInt = read

-- existing stocks reused for displaying battery status.
stockFromStatus OnMain = stockConnect
stockFromStatus (OnBattery s) = batteryStock s where
	batteryStock Normal    = stockYes
	batteryStock Warning   = "gtk-warning"
	batteryStock Low       = stockNo
	batteryStock Emergency = "gtk-error"

statusBat n
	| n <= thresholdEmergency  = Emergency
	| n <= thresholdLow        = Low
	| n <= thresholdWarning    = Warning
	| otherwise                = Normal
	
main = do
	_  <- initGUI

	{-
	pixels <- withImageSurface FormatARGB32 24 24 $ \surface -> do
		renderWith surface $ do
			setSourceRGB 1.0 0.0 0.0
			rectangle 2.0 2.0 6.0 6.0
		imageSurfaceGetPixels surface
	pixbufNew
	si <- statusIconNewFromPixbuf pixbuf
	-}

	start <- newIORef True
	status <- newIORef OnMain

	si <- statusIconNew
	statusIconSetFromStock si stockYes
	--on si statusIconActivate $ putStrLn "activated"
	--on si statusIconPopupMenu $ putStrLn "menu"

	statusIconSetTooltip si "getting power supplies status"
	_ <- flip timeoutAdd 1 $ do
		_ <- forkIO $ forever $ do
			mains <- enumMains
			bats  <- enumBatteries

			let r bat = do
				full <- readChargeFull bat
				now  <- readChargeNow bat
				st   <- readStatus bat
				let percent = fromIntegral now / fromIntegral full
				return (bat, percent :: Double, st == "Charging")
				
			z <- mapM r bats
			let allBats = sum $ map (\(_,x,_) -> x) z
			onMain <- or `fmap` mapM isOnline mains

			let tooltipBattery = concatMap (\(bat, p, c) -> printf "%s: %.0f%%%s\n" bat (p*100) (if c then " ↗" else " ↘")) z
			let tooltip = concat
				[ tooltipBattery
				, printf "total: %.0f%%\n" (100*allBats)
				, if onMain then "on main" else "on battery"
				]

			statusIconSetTooltip si tooltip

			current    <- readIORef status
			firstStart <- readIORef start
			let nextStatus = if onMain then OnMain else OnBattery $ statusBat (allBats * 100)
			unless (not firstStart && current == nextStatus) $ do
				statusIconSetFromStock si $ stockFromStatus nextStatus
				writeIORef status nextStatus
				writeIORef start False

			threadDelay (checkInterval * 1000000)
		return False
	mainGUI

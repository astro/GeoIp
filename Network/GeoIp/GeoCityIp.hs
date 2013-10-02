{-# LANGUAGE ExistentialQuantification #-}

-- | This module loads the MaxMind's GeoIp City database.
module Network.GeoIp.GeoCityIp (
	GeoCityDB,
	load,
	findRange,
	findLocation) where

import qualified Data.ByteString as B
import Data.Bits

import Network.GeoIp.GeoDB

data GeoCityDB = GeoCityDB GeoDB

-- | Load the city database.  If an error is triggered then
--   Left is returned with an error string.
load :: FilePath -> IO (Either String GeoCityDB)
load geoFile = do
	eGeo <- makeGeoDB geoFile
	case eGeo of
		Left e -> return $ Left e
		Right geo ->
			if dataBaseType geo == geoIPCountryEdition then
				return $ Right (GeoCityDB geo)
			else
				return $ Left ("load: Incorrect database type.  Database type is: " ++ (show $ dataBaseType geo))

extractRecordCity :: GeoDB -> Int -> (Double, Double)
extractRecordCity geo cursor =
	(latitude, longitude)
	where
		recordCursor = cursor + (2 * (recordLength geo) - 1) * (dataBaseSegments geo)
		memo = B.drop recordCursor (mem geo)
		(_, countryMem) = getBytes 1 memo
		(_, regionMem) = extractNullString countryMem
		(_, cityMem) = extractNullString regionMem
		(_, postalMem) = extractNullString cityMem
		(latitude, latMem) = ((fromIntegral ((getNumber $ B.take 3 postalMem)::Integer)) / 10000 - 180, B.drop 3 postalMem)
		(longitude, _) = ((fromIntegral ((getNumber $ B.take 3 latMem)::Integer)) / 10000 - 180, B.drop 3 latMem)


generateMask :: forall a. (Num a, Bits a) => Int -> Int -> a
generateMask from to =
	if from <= to then
		(bit from) .|. generateMask (from+1) to
	else
		(bit from)

-- | Find the IP range that the IP address is in.  The result is monadic.
--   In most cases you will want to use the Maybe monad.
findRange :: (Monad m) => GeoCityDB -> Integer -> m (Integer, Integer)
findRange (GeoCityDB geo) address = do
	(cursor, netMask) <- seekRecord geo address
	let
		bitMask = generateMask (31 - netMask) 31
		hostMask = generateMask 0 netMask
	if cursor == dataBaseSegments geo then
		fail "Could not find IP"
		else
			return (address .&. bitMask, address .|. hostMask)			
	
-- | Find the location of an IP address. The tuple returned is @(latitude, longitude)@.
--   The result is monadic, in most cases you will want to use the Maybe monad.
findLocation :: (Monad m) => GeoCityDB -> Integer -> m (Double, Double)
findLocation (GeoCityDB geo) address = do
	(cursor, _) <- seekRecord geo address
	if cursor == dataBaseSegments geo then
		fail "Could not find IP"
		else
			return (extractRecordCity geo cursor)
		

{-# LANGUAGE ExistentialQuantification,DeriveDataTypeable #-}
module Network.GeoIp.GeoDB where

import System.IO.Posix.MMap
import qualified Data.ByteString as B
import Data.Bits
import Data.Generics

data GeoDB = GeoDB {
	mem :: B.ByteString,
	dataBaseType :: Int,
	recordLength :: Int,
	dataBaseSegments :: Int
} deriving (Data, Typeable, Show)

makeGeoDB :: FilePath -> IO (Either String GeoDB)
makeGeoDB geoFile = do
	memo <- unsafeMMapFile geoFile
	let
		defaultGeo = GeoDB {mem = memo, dataBaseType = geoIPCountryEdition, recordLength = standardRecordLength, dataBaseSegments = countryBegin}
		eGeo = setupSegments defaultGeo ((B.length memo) - 3) 0
	return eGeo

setupSegments :: GeoDB -> Int -> Int -> Either String GeoDB
setupSegments geo cursor sizeRead
	| sizeRead >= structureMaxSize = Left "SetupSegments: could not find magic bytes. Incorrect file?"
	| otherwise = 
		if B.index (mem geo) cursor == 255 && B.index (mem geo) (cursor+1) == 255 && B.index (mem geo) (cursor+2) == 255 then
			let
				byteDbType = B.index (mem geo) (cursor+3)
				databaseType = if byteDbType >= 106 then (byteDbType - 105) else byteDbType
			in
				if databaseType == geoIPRegionEditionRev0 then
					Right geo { dataBaseSegments = stateBeginRev0 }
				else
					if databaseType == geoIPRegionEditionRev1 then
						Right geo { dataBaseSegments = stateBeginRev1 }
					else
						if (databaseType == geoIPCityEditionRev0 || databaseType == geoIPCityEditionRev1 || databaseType == geoIPOrgEdition || databaseType == geoIPIspEdition || databaseType == geoIPAsNumEdition) then
							let newgeo = geo {dataBaseSegments = getNumber $ B.take segmentRecordLength $ B.drop (cursor+4) (mem geo)}
							in
								if (databaseType == geoIPOrgEdition || databaseType == geoIPIspEdition) then
									Right newgeo {recordLength = orgRecordLength}
								else
									Right newgeo				
						else
							Right geo
		else
			setupSegments geo (cursor - 4) (sizeRead + 4)

getNumber :: forall b. (Bits b) => B.ByteString -> b
getNumber bytes = fst $ B.foldl combine (0, 0) bytes
	where
		combine (num, pos) b = (num + ((fromIntegral b) `shiftL` (pos*8)), pos + 1)

extractNullString :: B.ByteString -> (B.ByteString, B.ByteString)
extractNullString memo =
	(memTaken, memLeft)
	where
		memTaken = B.takeWhile (/=0) memo
		memLeft = B.drop ((B.length memTaken) + 1) memo

getBytes :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
getBytes num memo = (B.take num memo, B.drop num memo)

seekRecord :: forall a m. (Bits a, Monad m) => GeoDB -> a -> m (Int, Int)
seekRecord geo ipNum = seekRecord_ geo 31 0 ipNum

seekRecord_ :: forall a m. (Bits a, Monad m) => GeoDB -> Int -> Int -> a -> m (Int, Int)
seekRecord_ geo depth offset ipNum
	| depth < 0 = fail "Could not find record.  Database is possibly corrupt"
	| otherwise =
		if newOffset >= (dataBaseSegments geo) then
			return (newOffset, 32 - depth)
		else
			seekRecord_ geo (depth - 1) newOffset ipNum
		where
			cursor = (recordLength geo) * 2 * offset
			newOffset =
				if (ipNum .&. (1 `shiftL` depth) /= 0) then
					offsetFunction 1
				else
					offsetFunction 0
			offsetFunction pos = getNumber (B.take (recordLength geo) $ B.drop (cursor + (pos * (recordLength geo))) (mem geo))

geoIPCountryEdition :: forall n. (Num n) => n
geoIPCountryEdition = 1
geoIPRegionEditionRev0 :: forall n. (Num n) => n
geoIPRegionEditionRev0 = 7
geoIPCityEditionRev0 :: forall n. (Num n) => n
geoIPCityEditionRev0 = 6
geoIPOrgEdition :: forall n. (Num n) => n
geoIPOrgEdition = 5
geoIPIspEdition :: forall n. (Num n) => n
geoIPIspEdition = 4
geoIPCityEditionRev1 :: forall n. (Num n) => n
geoIPCityEditionRev1 = 2
geoIPRegionEditionRev1 :: forall n. (Num n) => n
geoIPRegionEditionRev1 = 3
--geoIPProxyEdition :: forall n. (Num n) => n
--geoIPProxyEdition = 8
geoIPAsNumEdition :: forall n. (Num n) => n
geoIPAsNumEdition = 9
--geoIPNetspeedEdition :: forall n. (Num n) => n
--geoIPNetspeedEdition = 10
--geoIPDomainEdition :: forall n. (Num n) => n
--geoIPDomainEdition = 11
structureMaxSize :: forall n. (Num n) => n
structureMaxSize = 20
segmentRecordLength :: forall n. (Num n) => n
segmentRecordLength = 3
standardRecordLength :: forall n. (Num n) => n
standardRecordLength = 3
orgRecordLength :: forall n. (Num n) => n
orgRecordLength = 4

stateBeginRev0 :: forall n. (Num n) => n
stateBeginRev0 = 16700000
stateBeginRev1 :: forall n. (Num n) => n
stateBeginRev1 = 16000000
countryBegin :: forall n. (Num n) => n
countryBegin = 16776960


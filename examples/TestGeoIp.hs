import Network.GeoIp.GeoCityIp

main :: IO ()
main = do
	eGeo <- load "GeoLiteCity.dat"
	case eGeo of
		Left e -> do
			putStrLn e
		Right geo -> do
			let
				mRange :: Maybe (Integer, Integer)
				mRange = findRange geo 3413704712
				mLocation :: Maybe (Double, Double)
				mLocation = findLocation geo 3413704712
			putStrLn $ show mRange
			putStrLn $ show mLocation
			putStrLn "Data from c library in February 2008 is: 203.121.0.8 -> (3.166700, 101.699997)"

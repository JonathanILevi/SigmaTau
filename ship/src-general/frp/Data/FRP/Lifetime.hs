module Data.FRP.Lifetime (Lifetime(..), newLifetime, kill) where

data Lifetime = Lifetime (IO ())
	
newLifetime :: IO () -> Lifetime
newLifetime k = Lifetime k

kill :: Lifetime -> IO ()
kill (Lifetime k) = k


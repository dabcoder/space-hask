{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import Network.HTTP.Conduit

-- | Launch data
data Launch = Launch { launch_date    :: String
                     , name           :: String
                     , r_type         :: String
                     , launch_success :: Bool
                     , details        :: String
                    } deriving (Show)

instance FromJSON Launch where
  parseJSON = withObject "Launch" $ \o -> do
    launch_date <- o .: "launch_date_local"
    launch_success <- o .: "launch_success"
    details <- o .: "details"
    rocket <- o .: "rocket"
    -- deconstruct rocket
    name <- rocket .: "rocket_name"
    r_type <- rocket .: "rocket_type"
    return Launch{..} 

baseURL :: String
baseURL = "https://api.spacexdata.com/v2"

latestURL :: String
latestURL = baseURL ++ "/launches/latest"

getLaunchData :: (FromJSON a) => String -> IO (Maybe a)
getLaunchData url = do
  obj <- simpleHttp latestURL
  return $ decode obj

main :: IO ()
main = do
  resp <- getLaunchData latestURL
  case resp of
   Nothing -> print $ "No data"
   Just (Launch{..}) -> do
    print $ "Launch date: " ++ launch_date
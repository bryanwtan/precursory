module Request where

import Network.HTTP.Simple (httpLBS)

get url = httpLBS url

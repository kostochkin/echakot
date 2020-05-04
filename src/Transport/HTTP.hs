module Transport.HTTP
    (  request
    ,  Request(..)
    , Protocol (..)
    , param
    ) where


data Protocol = Http | Https

type Param = (String, String)

data Request = Request
    { host :: String
    , port :: Integer
    , protocol :: Protocol
    , queryParams :: [Param]
    , postParams :: [Param]
    , body :: String
    }

param = (,)

request = undefined


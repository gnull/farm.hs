module Web where

import Control.Monad
import Happstack.Server (nullConf, simpleHTTP, ok, dir, seeOther)

serverStart :: IO ()
serverStart = simpleHTTP nullConf $ msum
    [ dir "hello"    $ ok "Hello, World!"
    , dir "goodbye"  $ ok "Goodbye, World!"
    , seeOther "/hello" "/hello"
    ]

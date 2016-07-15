{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static

main :: IO ()
main = scotty 3000 $ do

  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ file "./static/index.html"

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

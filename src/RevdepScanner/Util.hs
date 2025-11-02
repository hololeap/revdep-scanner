module RevdepScanner.Util
    ( encodeString
    ) where

import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

encodeString :: String -> BS.ByteString
encodeString = encodeUtf8 . T.pack

module RevdepScanner.Util
    ( encodeString
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

-- | Encode a 'String' directly into a strict 'BS.ByteString' using
--   'encodeUtf8'.
encodeString :: String -> BS.ByteString
encodeString = encodeUtf8 . T.pack

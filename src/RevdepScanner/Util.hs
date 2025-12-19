module RevdepScanner.Util
    ( encodeString
    , (%%)
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

-- | This is useful when we want to combine two 'NEMap's, but use the
--   underlying 'Semigroup' instance of the element on key collisions (as
--   opposed to clobbering it).
(%%) :: (Ord k, Semigroup a) => NEMap k a -> NEMap k a -> NEMap k a
m1 %% m2 = NEM.unionWith (<>) m1 m2

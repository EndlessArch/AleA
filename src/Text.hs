module Text where

import qualified Data.Text as T

splitWith :: T.Text -> T.Text -> (T.Text, T.Text)
splitWith x y =
  let (a, b) = T.breakOn x y
  in if T.null b
    then (a, b)
  else
    (a, T.drop (T.length x) b)
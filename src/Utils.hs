module Utils where

import Prelude (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

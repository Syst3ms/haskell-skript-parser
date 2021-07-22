{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SkriptParser.Util.Monad.Flow (
  Flow,
  mapFlow,
  (>>~)
) where

import Control.Monad.State (StateT(..), mapStateT)
import SkriptParser.Util.Wished (Wished(..))
import SkriptParser.Util.Monad.WishedT (WishedT(..), mapWishedT)

type Flow s = StateT s (WishedT IO)

mapFlow :: (Wished (a,s) -> Wished (b,s)) -> Flow s a -> Flow s b
mapFlow f = mapStateT (mapWishedT (fmap f))

(>>~) :: Flow s a -> (Wished (a,s) -> Wished (b,s)) -> Flow s b
(>>~) = flip mapFlow
{-# INLINE (>>~) #-}





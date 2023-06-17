module Block.Camera where

import Linear (V3)
import GHC.Generics (Generic)

data Camera = Camera 
  { pos :: V3 Float
  , front :: V3 Float
  , up :: V3 Float
  , yaw :: Float
  , pitch :: Float
  }
  deriving Generic

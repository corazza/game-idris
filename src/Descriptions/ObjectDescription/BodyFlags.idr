module Descriptions.ObjectDescription.BodyFlags

import GameIO
import Exception

public export
record BodyFlags where
  constructor MkBodyFlags
  drop : Bool

export
ObjectCaster BodyFlags where
  objectCast dict = with Checked do
    drop <- getBoolOrDefault False "drop" dict
    pure $ MkBodyFlags drop

export
defaultFlags : BodyFlags
defaultFlags = MkBodyFlags False

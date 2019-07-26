module Client.PClient

import JSONCache
import Descriptions.MapDescription
import Objects

public export
record PClient where
  constructor MkPClient
  preload : PreloadResults
  characterId : ObjectId

public export
fromMapPreload : ObjectId -> MapDescription -> PreloadResults -> PClient
fromMapPreload characterId map_description preload = MkPClient preload characterId

module Client.PClient

import JSONCache
import Descriptions.MapDescription
import Objects
import Settings

public export
record PClient where
  constructor MkPClient
  preload : PreloadResults
  characterId : ObjectId
  settings : ClientSettings

public export
record SessionData where
  constructor MkSessionData
  dummy : Int

export
emptySessionData : SessionData
emptySessionData = MkSessionData 0

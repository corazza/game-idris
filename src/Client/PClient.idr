module Client.PClient

import JSONCache
import Descriptions.MapDescription
import Objects
import Settings
import Timeline

public export
record PClient where
  constructor MkPClient
  preload : PreloadResults
  settings : ClientSettings
  character : Character

public export
record SessionData where
  constructor MkSessionData
  characterId : ObjectId

export
updateSettings : (f : ClientSettings -> ClientSettings) -> PClient -> PClient
updateSettings f = record { settings $= f }

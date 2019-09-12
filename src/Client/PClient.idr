module Client.PClient

import Client.UI.Inventory
import Client.UI.UICommand
import JSONCache
import GameIO
import Exception
import Descriptions.MapDescription
import Descriptions.ItemDescription
import Descriptions.SurfaceDescription
import Objects
import Settings
import Timeline

public export
record PClient where
  constructor MkPClient
  preload : PreloadResults
  settings : ClientSettings

public export
record SessionData where
  constructor MkSessionData
  characterId : ObjectId -- as an object in the scene
  character : Character -- needed to feed UI

export
updateSettings : (f : ClientSettings -> ClientSettings) -> PClient -> PClient
updateSettings f = record { settings $= f }

export
setCharacter : Character -> SessionData -> SessionData
setCharacter character' = record { character = character' }

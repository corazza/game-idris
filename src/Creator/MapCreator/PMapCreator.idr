module Creator.MapCreator.PMapCreator

import Physics.Vector2D

import JSONCache
import GameIO
import Descriptions.MapDescription
import Settings
import Commands
import Client.Rendering.Camera
import Client.Rendering.Layers
import Client.Rendering.PositionData
import Creator.MapCreator.Tools
import Creator.MapCreator.MapCreatorControl

public export
record PMapCreator where
  constructor MkPMapCreator
  idCounter : Nat
  preload : PreloadResults
  map_desc : Maybe MapDescription
  camera : Camera
  layers : Layers
  positions : Objects PositionData
  control : MapCreatorControl
  lastms : Int
  adding : AddingData
  tool : Maybe Tool
  mouseLast : Vector2D

defaultCamera : Camera
defaultCamera = fromSettings defaultCameraSettings

export
initialPMapCreator : Int -> PreloadResults -> PMapCreator
initialPMapCreator lastms preload
  = MkPMapCreator Z preload Nothing defaultCamera empty empty initialControl
                  lastms initialAddingData Nothing nullVector

export
setLastms : Int -> PMapCreator -> PMapCreator
setLastms lastms' = record { lastms = lastms' }

export
setMouseLast : Vector2D -> PMapCreator -> PMapCreator
setMouseLast pos = record { mouseLast = pos }

export
pmapSetTool : Tool -> PMapCreator -> PMapCreator
pmapSetTool tool = record { tool = Just $ tool }

export
pmapUnsetTool : PMapCreator -> PMapCreator
pmapUnsetTool = record { tool = Nothing }

export
pmapRotateAdding : Double -> PMapCreator -> PMapCreator
pmapRotateAdding angle = record { adding $= rotateAdding angle }

export
scounter : PMapCreator -> PMapCreator
scounter = record { idCounter $= S }

export
cameraSpeed : Double
cameraSpeed = 3

export
setMap : MapDescription -> PMapCreator -> PMapCreator
setMap desc = record { map_desc = Just desc }

export
updateControl : (f : MapCreatorControl -> MapCreatorControl) -> PMapCreator -> PMapCreator
updateControl f = record { control $= f }

export
queryControl : (q : MapCreatorControl -> a) -> PMapCreator -> a
queryControl q = q . control

export
updateCamera : (f : Camera -> Camera) -> PMapCreator -> PMapCreator
updateCamera f = record { camera $= f }

export
updateLayers : (f : Layers -> Layers) -> PMapCreator -> PMapCreator
updateLayers f = record { layers $= f }

export
queryLayers : (q : Layers -> a) -> PMapCreator -> a
queryLayers q = q . layers

export
addToPositions : ObjectId -> PositionData -> PMapCreator -> PMapCreator
addToPositions id positionData = record { positions $= addObject id positionData }

-- EDIT functions
export
editAddDynamic : Creation -> PMapCreator -> PMapCreator
editAddDynamic creation = record { map_desc $= map $ addDynamic creation }

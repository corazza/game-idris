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

public export
record AddingData where
  constructor MkAddingData
  ref : ContentReference
  angle : Double

initialAddingData : ContentReference -> AddingData
initialAddingData ref = MkAddingData ref 0.0

normalizedAngle : Double -> Double
normalizedAngle angle =
      let fills = the Int $ cast $ angle / (2*pi)
          in angle - (cast fills * 2 * pi)

normalizeAngle : AddingData -> AddingData
normalizeAngle = record { angle $= normalizedAngle }

rotateAdding : Double -> AddingData -> AddingData
rotateAdding angle' = normalizeAngle . (record { angle $= (+) angle' })

public export
record MapCreatorControl where
  constructor MkMapCreatorControl
  movingLeft : Bool
  movingRight : Bool
  movingUp : Bool
  movingDown : Bool

initialControl : MapCreatorControl
initialControl = MkMapCreatorControl False False False False

moveSelectors : MapCreatorControl -> List (Bool, Vector2D)
moveSelectors control =  [
  (movingLeft control, (-1, 0)),
  (movingRight control, (1, 0)),
  (movingUp control, (0, 1)),
  (movingDown control, (0, -1))
]

export
getMove : MapCreatorControl -> Vector2D
getMove control = let summed = sum $ map snd $ filter fst $ moveSelectors control
                      in if norm summed == 0 then nullVector else normed summed

export
startMoving : Direction -> MapCreatorControl -> MapCreatorControl
startMoving Left = record { movingLeft = True }
startMoving Right = record { movingRight = True }
startMoving Up = record { movingUp = True }
startMoving Down = record { movingDown = True }

export
stopMoving : Direction -> MapCreatorControl -> MapCreatorControl
stopMoving Left = record { movingLeft = False }
stopMoving Right = record { movingRight = False }
stopMoving Up = record { movingUp = False }
stopMoving Down = record { movingDown = False }

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
  adding : Maybe AddingData
  mouseLast : Vector2D

defaultCamera : Camera
defaultCamera = fromSettings defaultCameraSettings

export
initialPMapCreator : Int -> PreloadResults -> PMapCreator
initialPMapCreator lastms preload
  = MkPMapCreator Z preload Nothing defaultCamera empty empty initialControl
                  lastms Nothing nullVector

export
setLastms : Int -> PMapCreator -> PMapCreator
setLastms lastms' = record { lastms = lastms' }

export
setMouseLast : Vector2D -> PMapCreator -> PMapCreator
setMouseLast pos = record { mouseLast = pos }

export
pmapSetAdding : ContentReference -> PMapCreator -> PMapCreator
pmapSetAdding ref = record { adding = Just $ initialAddingData ref }

export
pmapUnsetAdding : PMapCreator -> PMapCreator
pmapUnsetAdding = record { adding = Nothing }

export
pmapRotateAdding : Double -> PMapCreator -> PMapCreator
pmapRotateAdding angle = record { adding $= map $ rotateAdding angle }

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

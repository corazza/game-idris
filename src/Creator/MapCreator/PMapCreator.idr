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
  positions : Objects (PositionData, Vector2D)
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
addToPositions : ObjectId -> PositionData -> Vector2D -> PMapCreator -> PMapCreator
addToPositions id positionData dims
  = record { positions $= addObject id (positionData, dims) }

-- export

-- EDIT functions
isClicked : Vector2D -> (ObjectId, PositionData, Vector2D) -> Bool
isClicked (at_x, at_y) (_, position_data, (w, h))
  = let (x, y) = position position_data
        in at_x > x-w && at_x < x+w && at_y > y-h && at_y < y+h

export
getIdAt : Objects (PositionData, Vector2D) -> Layers -> Vector2D -> Maybe ObjectId
getIdAt pos layers at = pickTop $ map fst $ filter (isClicked at) $ toList pos where
  sortWithLayer : (Nat, String) -> (Nat, String) -> Ordering
  sortWithLayer (x, _) (y, _) = compare y x

  filterNoLayer : List (Maybe Nat, String) -> List (Nat, String)
  filterNoLayer = foldr addConditional empty where
    addConditional : (Maybe Nat, String) -> List (Nat, String) -> List (Nat, String)
    addConditional (Nothing, _) acc = acc
    addConditional (Just x, id) acc = append (x, id) acc

  pickTop : List ObjectId -> Maybe ObjectId
  pickTop xs = let layerNums = map (flip getLayer layers) xs
                   withLayer = filterNoLayer $ zip layerNums xs
                   sorted = sortBy sortWithLayer withLayer
                   in map snd $ head' sorted

export
editAddDynamic : Creation -> PMapCreator -> PMapCreator
editAddDynamic creation = record { map_desc $= map $ addDynamic creation }

module Creator.MapCreator.PMapCreator

import Physics.Vector2D

import JSONCache
import GameIO
import Descriptions.MapDescription
import Descriptions.Color
import Descriptions.ObjectDescription.RenderDescription
import Settings
import Commands
import Client.Rendering.Camera
import Client.Rendering.Layers
import Client.Rendering.PositionData
import Client.Rendering.Transforms
import Creator.MapCreator.Tools
import Creator.MapCreator.MapCreatorControl

export
objBackground : Color
objBackground = MkColor 165 103 255 100

export
wallBackground : Color
wallBackground = MkColor 0 171 84 100

export
spawnColor : Color
spawnColor = MkColor 55 8 200 120

-- TODO remove control!!!! concerns!!!
-- Creator should have a concept of "views", where the content of editors
-- can be rendered, and a single camera per view or something

public export
record PMapCreator where
  constructor MkPMapCreator
  idCounter : Nat
  preload : PreloadResults
  map_desc : Maybe MapDescription
  camera : Camera
  layers : Layers
  invisibles : Objects RenderMethod
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
  = MkPMapCreator Z preload Nothing defaultCamera empty empty empty initialControl
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
updateAdding : (f : AddingData -> AddingData) -> PMapCreator -> PMapCreator
updateAdding f = record { adding $= f }

export
queryAdding : (q : AddingData -> a) -> PMapCreator -> a
queryAdding q = q . adding

export
scounter : PMapCreator -> PMapCreator
scounter = record { idCounter $= S }

export
cameraSpeed : Double
cameraSpeed = 3

export
setMap : MapDescription -> PMapCreator -> PMapCreator
setMap desc = record { map_desc = Just desc, idCounter = getIdNum desc }

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
addToInvisibles : ObjectId -> RenderMethod -> PMapCreator -> PMapCreator
addToInvisibles id m = record { invisibles $= addObject id m }

export
removeFromInvisibles : ObjectId -> PMapCreator -> PMapCreator
removeFromInvisibles id = record { invisibles $= removeObject id }

export
addToPositions : ObjectId -> PositionData -> Vector2D -> PMapCreator -> PMapCreator
addToPositions id positionData dims
  = record { positions $= addObject id (positionData, dims) }

export
removeFromPositions : ObjectId -> PMapCreator -> PMapCreator
removeFromPositions id = record { positions $= removeObject id }

export
posdims : ObjectId -> PMapCreator -> Maybe (PositionData, Vector2D)
posdims id = lookup id . positions

export
updateMap : (f : MapDescription -> MapDescription) -> PMapCreator -> PMapCreator
updateMap f = record { map_desc $= map f }

export -- will only be used by MapCreator so defined here
setDynamic : List Creation -> MapDescription -> MapDescription
setDynamic xs = record { creations = xs }

export
setPosition : ObjectId -> Vector2D -> PMapCreator -> PMapCreator
setPosition id pos = record { positions $= updateObject id setPosition' } where
  setPosition' : (PositionData, Vector2D) -> (PositionData, Vector2D)
  setPosition' (posdata, dims) = (setPosition pos posdata, dims)

export
setAngle : ObjectId -> Double -> PMapCreator -> PMapCreator
setAngle id x = record { positions $= updateObject id setAngle' } where
  setAngle' : (PositionData, Vector2D) -> (PositionData, Vector2D)
  setAngle' (posdata, dims) = (setAngle x posdata, dims)

-- EDIT functions
export
pmapGetIdAt : Objects (PositionData, Vector2D) -> Layers -> Vector2D -> Maybe ObjectId
pmapGetIdAt pos layers at = pickTop $ map fst $ filter (isClicked at) $ toList pos where
  isClicked : Vector2D -> (ObjectId, PositionData, Vector2D) -> Bool
  isClicked click_pos@(at_x, at_y) (_, position_data, (w, h))
    = let angle = angle position_data
          pos@(x, y) = position position_data
          dims = (w, h)
          rect = makeRotatedRect angle pos dims
          rotated_rect = rotatePoints (negate angle) nullVector rect
          [(rat_x, rat_y)] = rotatePoints (negate angle) nullVector [click_pos]
          ((rx, ry), (rw, rh)) = getAABB rotated_rect
          in rat_x > rx-rw && rat_x < rx+rw && rat_y > ry-rh && rat_y < ry+rh

  isClicked' : Vector2D -> (ObjectId, PositionData, Vector2D) -> Bool
  isClicked' (at_x, at_y) (_, position_data, (w, h))
    = let (x, y) = position position_data
          in at_x > x-w && at_x < x+w && at_y > y-h && at_y < y+h

  sortWithLayer : (Nat, String) -> (Nat, String) -> Ordering
  sortWithLayer (x, _) (y, _) = compare y x

  filterNoLayer : List (Maybe Nat, String) -> List (Nat, String)
  filterNoLayer = foldr addConditional empty where
    -- objects without a layer regarded as layer Z
    addConditional : (Maybe Nat, String) -> List (Nat, String) -> List (Nat, String)
    addConditional (Nothing, id) acc = append (Z, id) acc
    addConditional (Just x, id) acc = append (x, id) acc

  pickTop : List ObjectId -> Maybe ObjectId
  pickTop xs = let layerNums = map (flip getLayer layers) xs
                   withLayer = filterNoLayer $ zip layerNums xs
                   sorted = sortBy sortWithLayer withLayer
                   in map snd $ head' sorted

export
getWallPosDims : (beginpos : Vector2D) -> (endpos : Vector2D) -> (Vector2D, Vector2D)
getWallPosDims beginpos endpos = let dimv = 0.5 `scale` (endpos - beginpos)
                                     in (beginpos + dimv, abs dimv)

export
editAddDynamic : Creation -> PMapCreator -> PMapCreator
editAddDynamic creation = record { map_desc $= map $ addDynamic creation }

export
editRemoveDynamic : ObjectId -> PMapCreator -> PMapCreator
editRemoveDynamic id = record { map_desc $= map $ removeDynamic id }

export
editAddStatic : StaticCreation -> PMapCreator -> PMapCreator
editAddStatic creation = record { map_desc $= map $ addStatic creation }

export
editRemoveStatic : ObjectId -> PMapCreator -> PMapCreator
editRemoveStatic id = record { map_desc $= map $ removeStatic id }

export
editSetSpawn : Vector2D -> PMapCreator -> PMapCreator
editSetSpawn pos = record { map_desc $= map $ setSpawn pos }

module Creator.MapCreator.Tools

import Physics.Vector2D

import JSONCache
import GameIO

public export
record AddingData where
  constructor MkAddingData
  angle : Double
  selectBegin : Maybe Vector2D
  clickedOn : Maybe ContentReference

export
initialAddingData : AddingData
initialAddingData = MkAddingData 0.0 Nothing Nothing

export
normalizedAngle : Double -> Double
normalizedAngle angle =
      let fills = the Int $ cast $ angle / (2*pi)
          in angle - (cast fills * 2 * pi)

export
normalizeAngle : AddingData -> AddingData
normalizeAngle = record { angle $= normalizedAngle }

export
rotateAdding : Double -> AddingData -> AddingData
rotateAdding angle' = normalizeAngle . (record { angle $= (+) angle' })

export
setAngle : Double -> AddingData -> AddingData
setAngle x = record { angle = x }

export
setSelectBegin : Vector2D -> AddingData -> AddingData
setSelectBegin vec = record { selectBegin = Just vec }

export
unsetSelectBegin : AddingData -> AddingData
unsetSelectBegin = record { selectBegin = Nothing }

export
setClickedOn' : Maybe ContentReference -> AddingData -> AddingData
setClickedOn' x = record { clickedOn = x }

export
unsetClickedOn : AddingData -> AddingData
unsetClickedOn = record { clickedOn = Nothing }

public export
data Tool = Add ContentReference
          | Remove
          | AddRectWall
          | SetSpawn
          | Move

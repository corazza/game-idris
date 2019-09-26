module Creator.MapCreator.Tools

import Physics.Vector2D

import JSONCache
import GameIO

public export
record AddingData where
  constructor MkAddingData
  angle : Double

export
initialAddingData : AddingData
initialAddingData = MkAddingData 0.0

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

public export
data Tool = Add ContentReference
          | Remove

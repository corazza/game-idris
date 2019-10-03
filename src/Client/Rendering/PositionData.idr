module Client.Rendering.PositionData

import Physics.Vector2D
import Dynamics.BodyData
import Dynamics.MoveDirection
import Descriptions.MapDescription
import Objects

public export
record PositionData where
  constructor MkPositionData
  position : Vector2D
  angle : Double -- in degrees
  flip : Int

getFlip : BodyData -> Int
getFlip body_data = case forceDirection body_data of
  Leftward => 2
  Rightward => 0

export
radToDeg : Double -> Double
radToDeg angle = - angle / (2.0*pi) * 360.0

export
degToRad : Double -> Double
degToRad angle = - (angle / 360.0) * 2.0*pi

getDegAngle : BodyData -> Double
getDegAngle = radToDeg . angle

fromBodyData : BodyData -> PositionData
fromBodyData body_data = MkPositionData
  (position body_data) (getDegAngle body_data) (getFlip body_data)

export
bodyDataToPositionData : Objects BodyData -> Objects PositionData
bodyDataToPositionData = map fromBodyData

export
fromCreation : Creation -> PositionData
fromCreation creation = MkPositionData
  (position creation) (fromMaybe 0 $ angle creation) 0

export
fromCreation' : StaticCreation -> PositionData
fromCreation'  = fromCreation . creation

export
noFlip : Vector2D -> Double -> PositionData
noFlip pos angle = MkPositionData pos (radToDeg angle) 0

module Physics

import Control.ST
import Data.AVL.Dict

import Vector2D

public export
Force : Type
Force = Vector2D

public export
PositionUpdate : Type
PositionUpdate = (String, Vector2D)

public export
data CollisionEvent = Start String String | Stop String String

public export
data BoxType = Static | Dynamic

public export
record BoxDescription where
  constructor MkBoxDescription
  mass : Double
  type : BoxType
  dim : Vector2D

public export
record Box where
  constructor MkBox
  id : String
  description : BoxDescription
  position : Vector2D
  velocity : Vector2D
  forcesDict : Dict String Vector2D

Show Box where
  show box = "(id = " ++ show (Box.id box) ++
             ", pos = " ++ show (position box) ++
             ", vel = " ++ show (velocity box) ++
             ", forces = " ++ showForces box ++ ")" where
     showForces : Box -> String
     showForces box = show $ values (forcesDict box)

public export
data Bounds = NoBounds | HardBounds Double Double | ErasureBounds Double Double

-- TODO abstract away "ST container with IDs maybe?"
export
SPhysics : Type
SPhysics = Composite [State (Dict String Box),
                      State (Dict String Force)]

export
initPhysics : (bounds : Bounds) -> ST m Var [add SPhysics]
initPhysics bounds = with ST do
  boxes <- new (the (Dict String Box) empty)
  forces <- new (the (Dict String Force) empty)
  physics <- new ()
  combine physics [boxes, forces]
  pure physics

export
quitPhysics : (physics : Var) -> ST m () [remove physics SPhysics]
quitPhysics physics = with ST do
  [boxes, forces] <- split physics
  delete boxes; delete forces
  delete physics

export
addBox : (physics : Var) -> (box : Box) -> ST m () [physics ::: SPhysics]
addBox physics box = with ST do
  [boxes, forces] <- split physics
  write boxes (insert (id box) box !(read boxes))
  combine physics [boxes, forces]

export
setForce : ConsoleIO m =>
           (physics : Var) ->
           (boxId : String) ->
           (forceName : String) ->
           (force : Force) ->
           ST m () [physics ::: SPhysics]
setForce physics boxId forceName force = with ST do
  [boxes, forces] <- split physics
  boxesDict <- read boxes
  case lookup boxId boxesDict of
       Nothing => combine physics [boxes, forces]
       Just box => with ST do
         let updated = Dict.update boxId (record {
              forcesDict $= Dict.update forceName (const force) }) boxesDict
         write boxes updated
         combine physics [boxes, forces]

export
setGlobalForce : (physics : Var) ->
                 (forceName : String) ->
                 (force : Force) ->
                 ST m () [physics ::: SPhysics]
setGlobalForce physics forceName force = with ST do
  [boxes, forces] <- split physics
  forcesDict <- read forces
  write forces (Dict.update forceName (const force) forcesDict)
  combine physics [boxes, forces]

export
iterate : ConsoleIO m => (physics : Var) -> (dt : Double) ->
          ST m (List PositionUpdate, List CollisionEvent) [physics ::: SPhysics]
iterate physics dt = with ST do
  [boxes, forces] <- split physics
  boxesDict <- read boxes
  globalForcesDict <- read forces
  let updatedVelocity = map (\box => record {
    velocity = sum (forcesDict box) + sum globalForcesDict } box ) boxesDict
  let updatedPosition = map (\box => record {
    position $= (+ dt `scale` (velocity box)) } box ) updatedVelocity
  write boxes updatedPosition
  combine physics [boxes, forces]
  -- printLn (values updatedPosition)
  pure (toList (map position updatedPosition), [])

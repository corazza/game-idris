module Physics

import Control.ST
import Data.AVL.Dict

import Vector2D

public export
Force : Type
Force = Vector2D

emptyForceDict : Dict String Force
emptyForceDict = empty

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
data Bounds = HardBounds Double Double
            | ErasureBounds Double Double
            | Floor Double

public export
record Parameters where
  constructor MkParameters
  gravity : Double
  bounds : Maybe Bounds

public export
record Box where
  constructor MkBox
  id : String
  description : BoxDescription
  position : Vector2D
  velocity : Vector2D
  accel : Vector2D
  forcesDict : Dict String Vector2D

%name Box box

emptyBoxDict : Dict String Box
emptyBoxDict = empty

mass : Box -> Double
mass = mass . description

resultant : (global : Force) -> Box -> Force
resultant global box = global + (sum  (forcesDict box))

Show Box where
  show box = "(id = " ++ show (Box.id box) ++
             ", pos = " ++ show (position box) ++
             ", vel = " ++ show (velocity box) ++
             ", accel = " ++ show (accel box) ++
             ", forces = " ++ showForces box ++ ")" where
     showForces : Box -> String
     showForces box = show $ values (forcesDict box)


-- TODO abstract away "ST container with IDs maybe?"
export
SPhysics : Type
SPhysics = Composite [State (Dict String Box),
                      State (Dict String Force),
                      State Parameters]


export
addBox : (physics : Var) -> (box : Box) -> ST m () [physics ::: SPhysics]
addBox physics box = with ST do
  [boxes, forces, params] <- split physics
  write boxes (insert (id box) box !(read boxes))
  combine physics [boxes, forces, params]

export
initPhysics : (params : Parameters) -> ST m Var [add SPhysics]
initPhysics params = (with ST do
  boxes <- new emptyBoxDict
  forces <- new emptyForceDict
  sparams <- new params
  physics <- new ()
  combine physics [boxes, forces, sparams]
  addBounds physics (bounds params)
  pure physics) where
    createBounds : Bounds -> Box
    createBounds (HardBounds x y) = ?createBounds_rhs_2
    createBounds (ErasureBounds x y) = ?createBounds_rhs_3
    createBounds (Floor x) = MkBox "__physics_bounds"
                                   (MkBoxDescription 1 Static (10000, 1))
                                   (0, x)
                                   nullVector
                                   nullVector
                                   emptyForceDict

    addBounds : (physics : Var) -> Maybe Bounds -> ST m () [physics ::: SPhysics]
    addBounds physics Nothing = pure ()
    addBounds physics (Just x) = addBox physics (createBounds x)

export
quitPhysics : (physics : Var) -> ST m () [remove physics SPhysics]
quitPhysics physics = with ST do
  [boxes, forces, params] <- split physics
  delete boxes; delete forces; delete params
  delete physics

export
setForce : ConsoleIO m =>
           (physics : Var) ->
           (boxId : String) ->
           (forceName : String) ->
           (force : Force) ->
           ST m () [physics ::: SPhysics]
setForce physics boxId forceName force = with ST do
  [boxes, forces, params] <- split physics
  boxesDict <- read boxes
  case lookup boxId boxesDict of
       Nothing => combine physics [boxes, forces, params]
       Just box => with ST do
         let updated = Dict.update boxId (record {
              forcesDict $= Dict.update forceName (const force) }) boxesDict
         write boxes updated
         combine physics [boxes, forces, params]

export
setGlobalForce : (physics : Var) ->
                 (forceName : String) ->
                 (force : Force) ->
                 ST m () [physics ::: SPhysics]
setGlobalForce physics forceName force = with ST do
  [boxes, forces, params] <- split physics
  forcesDict <- read forces
  write forces (Dict.update forceName (const force) forcesDict)
  combine physics [boxes, forces, params]

export
iterate : ConsoleIO m => (physics : Var) -> (dt : Double) ->
          ST m (List PositionUpdate, List CollisionEvent) [physics ::: SPhysics]
iterate physics dt = with ST do
  [boxes, forces, sparams] <- split physics
  boxesDict <- read boxes
  globalForcesDict <- read forces
  params <- read sparams
  let resultant' = resultant $ sum globalForcesDict
  let updatedAccel = map (\box => record {
    accel = (0, gravity params) + ((1.0 / (mass box)) `scale` (resultant' box)) } box ) boxesDict
  let updatedVelocity = map (\box => record {
    velocity $= (+ dt `scale` (accel box)) } box ) updatedAccel
  let updatedPosition = map (\box => record {
    position $= (+ dt `scale` (velocity box)) } box ) updatedVelocity
  write boxes updatedPosition
  combine physics [boxes, forces, sparams]
  pure (toList (map position updatedPosition), [])

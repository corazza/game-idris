module Objects

import Graphics.SDL2
import Data.AVL.Set
import Data.AVL.Dict
import Physics.Vector2D
import Control.ST
import Exception

import Physics.Box2D
import Descriptors
import Resources
import Common
import Script
import Data.AVL.DDict

%access public export

data AnimationState = MkAnimationState String Int -- state, started (ms)

initialAnimationState : AnimationState
initialAnimationState = MkAnimationState "resting" 0

advanceAnimation : Int -> AnimationState -> AnimationState
advanceAnimation ticks (MkAnimationState state current)
  = MkAnimationState state (current+ticks)

data CompleteRenderDescriptor
  = DrawBox ResourceReference Vector2D
  | TileWith ResourceReference Vector2D (Nat, Nat)
  | Animated (Dict String AnimationParameters) AnimationState
  | Invisible

decideRenderDescription : IncompleteRenderDescriptor -> CreationData ->
                          Maybe CompleteRenderDescriptor
decideRenderDescription Invisible _ = Just Invisible
decideRenderDescription (DrawBox x dims) (BoxData y) = Just $ DrawBox x dims
decideRenderDescription (TileWith tileRef (x, y)) (WallData (nx, ny))
  = Just $ TileWith tileRef (x, y) (cast nx, cast ny)
decideRenderDescription (Animated states) (BoxData impulse)
  = Just $ Animated states initialAnimationState
decideRenderDescription _ _ = Nothing

decideRenderDescription' : IncompleteRenderDescriptor -> CreationData ->
                           STrans m (Maybe CompleteRenderDescriptor) xs (const xs)
decideRenderDescription' x y = pure $ decideRenderDescription x y

data MoveDirection = Leftward | Rightward
%name MoveDirection direction

Eq MoveDirection where
  Leftward == Leftward = True
  Rightward == Rightward = True
  _ == _ = False

Show MoveDirection where
  show Leftward = "left"
  show Rightward = "right"

record ControlState where
  constructor MkControlState
  moving : List MoveDirection
  facing : MoveDirection
  jumping : Bool
  canJump : Bool
  attacking : Bool
%name ControlState controlState

Show ControlState where
  show (MkControlState moving facing jumping canJump attacking)
    = "(moving: " ++ show moving ++ "," ++
      " facing: " ++ show facing ++ "," ++
      " jumping: " ++ show jumping ++ "," ++
      " attacking: " ++ show attacking ++ ")"

noControl : MoveDirection -> ControlState
noControl facing = MkControlState empty facing False False False

moveSign : ControlState -> Double
moveSign controlState = case moving controlState of
  [] => 0
  Leftward :: _ => -1
  Rightward :: _ => 1

moveToTop : Eq a => a -> List a -> List a
moveToTop x xs = x :: filter (/= x) xs

startMoving : (direction : MoveDirection) -> ControlState -> ControlState
startMoving direction = record { moving $= moveToTop direction, facing = direction }

stopMoving : (direction : MoveDirection) -> ControlState -> ControlState
stopMoving direction ctst
  = let newCtst = record { moving $= filter (/= direction) } ctst
        in case moving newCtst of
          [] => newCtst
          x :: _ => record {facing=x} newCtst

stopDead : ControlState -> ControlState
stopDead = record { moving = empty }

startJumping : ControlState -> ControlState
startJumping = record { jumping = True }

stopJumping : ControlState -> ControlState
stopJumping = record { jumping = False, canJump = True }

startAttacking : ControlState -> ControlState
startAttacking = record { attacking = True }

stopAttacking : ControlState -> ControlState
stopAttacking = record { attacking = False }

resetControlState : ControlState -> ControlState
resetControlState ctst = record { canJump = not (jumping ctst) } ctst

record PhysicsProperties where
  constructor MkPhysicsProperties
  type : BodyType
  position : Vector2D
  velocity : Vector2D
  angle : Double
  mass : Double -- overwritten on Scene.addWithId
  touching : Set ObjectId
  fixedRotation : Maybe Bool
  bullet : Maybe Bool
  fixtures : List FixtureDefinition

-- TODO make a Scripts : Type, possibly move to Scripts.idr, which handles
-- adding/deactivation etc. ScriptHolder interface both for Scripts and Object
record Scripts where
  constructor MkScripts
  attack : Maybe (ActionParameters -> UnitScript)
  collisions : DDict String (CollisionData -> UnitScript)

noScripts : Scripts
noScripts = MkScripts Nothing empty

decideScripts : ObjectDescriptor -> Creation -> Scripts
decideScripts desc creation = MkScripts (decideAttack desc) (decideCollisions desc creation)

export
activeCollisions : Scripts -> List (CollisionData -> UnitScript)
activeCollisions = values . collisions

interface Damagable a where
  takeDamage : Double -> a -> a
  alive : a -> Bool

record Health where
  constructor MkHealth
  current : Double
  full : Double
%name Health health

Damagable Health where
  takeDamage x = record { current $= \y => y - x }
  alive health = (current health) > 0

Show Health where
  show (MkHealth current full) =
    "{ current: " ++ show current ++ ", full: " ++ show full ++ " }"

fromFull : Double -> Health
fromFull x = MkHealth x x

percent : Health -> Double
percent (MkHealth current full) = current / full

record ObjectControl where
  constructor MkObjectControl
  controlState : ControlState
  controlParameters : ControlDescriptor
%name ObjectControl control

Show ObjectControl where
  show (MkObjectControl ctst ctp)
    =  "{ control | "
    ++   "controlState: " ++ show ctst
    ++ ", controlParameters: " ++ show ctp
    ++ "}"

resetObjectControl : ObjectControl -> ObjectControl
resetObjectControl = record { controlState $= resetControlState }

updateObjectControl : (f : ControlState -> ControlState) -> ObjectControl -> ObjectControl
updateObjectControl f = record { controlState $= f }

record Object where
  constructor MkObject
  id : String
  name : String
  physicsProperties : PhysicsProperties
  renderDescription : CompleteRenderDescriptor
  tags : Set ObjectTag
  scripts : Scripts
  control : Maybe ObjectControl
  health : Maybe Health -- TODO move health, controlState, and tags into components
  effects : List PhysicsEffect
%name Object object

Show Object where
  show (MkObject id name physicsProperties renderDescription tags scripts control health _)
    =  "{ object | "
    ++   "id: " ++ id
    ++ ", name: " ++ name
    ++ ", tags: " ++ show tags
    ++ ", control: " ++ show control
    ++ ", health: " ++ show health
    ++ " }"

Damagable Object where
  takeDamage x = record { health $= map (takeDamage x) }
  alive object = case health object of
    Nothing => True
    Just health => alive health

controlState : Object -> Maybe ControlState
controlState = map controlState . control

facing : Object -> Maybe MoveDirection
facing = map facing . controlState

forceDirection : Object -> MoveDirection
forceDirection object = case facing object of
  Nothing => Rightward
  Just x => x

animationState : Object -> String
animationState object = case controlState object of
  Nothing => "resting"
  Just ctst => case moving ctst of
    [] => "resting"
    _ => "moving"

updateAnimationState : (ticks : Int) -> Object -> Object
updateAnimationState ticks object = case renderDescription object of
  Animated states state@(MkAnimationState previousState soFar) =>
    let currentState = animationState object
        in case previousState == currentState of
          False => let newState = MkAnimationState currentState 0 in
            record {renderDescription = Animated states newState} object
          True =>  record {
            renderDescription = Animated states (advanceAnimation ticks state)
          } object
  _ => object

controlParameters : Object -> Maybe ControlDescriptor
controlParameters = map controlParameters . control

deactivateCollision : (name : String) -> Object -> Object
deactivateCollision name = record { scripts->collisions $= delete name }

physicsUpdate : (PhysicsProperties -> PhysicsProperties) -> Object -> Object
physicsUpdate f = record { physicsProperties $= f }

jumping : Object -> Bool
jumping object = case control object of
  Nothing => False
  Just x => jumping $ controlState x

resetControl : Object -> Object
resetControl = record { control $= map resetObjectControl }

updateControl : (f : ControlState -> ControlState) -> Object -> Object
updateControl f = record { control $= map (updateObjectControl f) }

touching : Object -> Set ObjectId
touching = touching . physicsProperties

fixedRotation : Object -> Maybe Bool
fixedRotation = fixedRotation . physicsProperties

addTouching : ObjectId -> Object -> Object
addTouching id = physicsUpdate $ record { touching $= insert id }

removeTouching : ObjectId -> Object -> Object
removeTouching id = let to_remove = insert id empty in
  physicsUpdate $ record { touching $= \t => difference t to_remove }

-- TODO delete
getControlST : Object -> STrans m (Maybe ControlDescriptor) xs (const xs)
getControlST = pure . controlParameters

angle : Object -> Double
angle = angle . physicsProperties

mass : Object -> Double
mass = mass . physicsProperties

dimensions : Object -> Vector2D
dimensions object = case renderDescription object of
  DrawBox ref dim => dim
  TileWith ref (x, y) (nx, ny) => (cast nx * x * 0.5, cast ny * y * 0.5)
  Animated states state => let state = animationState object in case lookup state states of
    Nothing => nullVector
    Just aparams => dimensions aparams
  Invisible => case fixtures $ physicsProperties object of
    [] => nullVector
    (x :: xs) => case shape x of
      (Circle r) => (r, r)
      (Box x) => x
      (Polygon xs) => ?polygonDimensionsObjects

dim : Object -> Vector2D
dim = dimensions

position : Object -> Vector2D
position = position . physicsProperties

velocity : Object -> Vector2D
velocity = velocity . physicsProperties

type : Object -> BodyType
type = type . physicsProperties

bullet : Object -> Maybe Bool
bullet = bullet . physicsProperties

onGround : Object -> Bool
onGround object = size (touching object) > 0

movementImpulse : Object -> Vector2D
movementImpulse object = case control object of
  Nothing => nullVector
  Just (MkObjectControl controlState controlParameters) => let
    (x, y) = velocity object
    x' = (speed controlParameters) * (moveSign controlState)
    y' = if jumping controlState && canJump controlState && (onGround object)
              then jump controlParameters else 0
    x_correction = if abs (x' - x) < 0.01 then 0 else x' - x
          in (mass object) `scale` (x_correction, y')

x : Object -> Double
x = fst . position

y : Object -> Double
y = snd . position

controlFromDescriptor : ObjectDescriptor -> Maybe ObjectControl
controlFromDescriptor = map (MkObjectControl (noControl Rightward)) . control

fromParams : FixtureParameters -> Shape -> FixtureDefinition
fromParams (MkFixtureParameters offset angle density friction restitution) shape
  = MkFixtureDefinition shape offset angle density friction restitution

fromShapes : List ShapedFixtureDescriptor -> List FixtureDefinition
fromShapes [] = []
fromShapes ((MkShapedFixtureDescriptor shape params) :: xs)
  = fromParams params shape :: fromShapes xs

getPhysicsProperties : BodyDescriptor ->
                       Creation ->
                       IncompleteRenderDescriptor ->
                       Maybe PhysicsProperties
getPhysicsProperties bdesc creation irdesc = with Maybe do
  let cdata = creationData creation
  let noFixtures = MkPhysicsProperties
    (type bdesc) (position creation) nullVector (angle creation) (-1) empty
    (fixedRotation bdesc) (bullet bdesc)
  case fixtures bdesc of
    Left (MkImmaterialFixtureDescriptor params) => case cdata of
      BoxData x => Nothing
      WallData (nx, ny) => case irdesc of
        TileWith _ (x, y) => pure $
          noFixtures $ [fromParams params (Box (cast nx * x, cast ny * y))]
        _ => Nothing
      InvisibleWallData x => pure $ noFixtures $ [fromParams params (Box x)]
    Right [] => Nothing
    Right shapes => pure $ noFixtures $ fromShapes shapes

fromDescriptorCreation : ObjectDescriptor -> Creation -> Maybe Object
fromDescriptorCreation desc creation = with Maybe do
  let id = getOrDefault "" (id creation)
  let tags = tags creation `union` tags desc
  let health = map fromFull (health desc)
  let scripts = decideScripts desc creation
  let irdesc = renderDescription desc
  let cdata = creationData creation
  let bdesc = bodyDescription desc
  crdesc <- decideRenderDescription irdesc cdata
  physics_properties <- getPhysicsProperties bdesc creation irdesc
  pure $ MkObject id (name desc) physics_properties crdesc tags scripts
                  (controlFromDescriptor desc) health (effects bdesc)


fromDescriptorCreation' : ObjectDescriptor -> Creation ->
                          STrans m (Maybe Object) xs (const xs)
fromDescriptorCreation' desc creation = pure $ fromDescriptorCreation desc creation

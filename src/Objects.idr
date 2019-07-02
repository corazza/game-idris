module Objects

import Graphics.SDL2
import Data.AVL.Set
import Physics.Vector2D
import Control.ST

import Physics.Box2D
import Descriptors
import Resources
import Common
import Script

import Data.AVL.DDict

%access public export

data CompleteRenderDescriptor
  = DrawBox ResourceReference Vector2D
  | TileWith ResourceReference Vector2D (Nat, Nat)
  | Invisible

decideRenderDescription : IncompleteRenderDescriptor -> CreationData ->
                          Maybe CompleteRenderDescriptor
decideRenderDescription Invisible _ = Just Invisible
decideRenderDescription (DrawBox x dims) (BoxData y) = Just $ DrawBox x dims
decideRenderDescription (TileWith tileRef (x, y)) (WallData (nx, ny))
  = Just $ TileWith tileRef (x, y) (cast nx, cast ny)
decideRenderDescription _ _ = Nothing

decideRenderDescription' : IncompleteRenderDescriptor -> CreationData ->
                           STrans m (Maybe CompleteRenderDescriptor) xs (const xs)
decideRenderDescription' x y = pure $ decideRenderDescription x y

data MoveDirection = Leftward | Rightward
%name MoveDirection direction

Show MoveDirection where
  show Leftward = "left"
  show Rightward = "right"

record ControlState where
  constructor MkControlState
  moving : Maybe MoveDirection
  jumping : Bool
  canJump : Bool
  attacking : Bool
%name ControlState controlState

Show ControlState where
  show (MkControlState moving jumping canJump attacking)
    = "(moving: " ++ (show moving) ++ "," ++
      " jumping: " ++ (show jumping) ++ "," ++
      " attacking: " ++ (show attacking) ++ ")"

noControl : ControlState
noControl = MkControlState Nothing False False False

moveSign : ControlState -> Double
moveSign controlState = case moving controlState of
  Nothing => 0
  Just Leftward => -1
  Just Rightward => 1

startMoving : (direction : MoveDirection) -> ControlState -> ControlState
startMoving direction = record { moving = Just direction }

stopMoving : ControlState -> ControlState
stopMoving = record { moving = Nothing }

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

-- mass overwritten on addBody
record PhysicsProperties where
  constructor MkPhysicsProperties
  type : BodyType
  position : Vector2D
  velocity : Vector2D
  angle : Double
  -- density : Double
  -- friction : Double
  mass : Double -- overwritten on Scene.addWithId
  touching : Set ObjectId
  fixtures : List FixtureDefinition
  -- dimensions : Vector2D

-- fromDescCreation : BodyDescriptor -> Creation -> PhysicsProperties
-- fromDescCreation desc creation
--   = let noFixtures = MkPhysicsProperties (type desc) (position creation)
--                                          nullVector (angle creation) (-1) empty
--       in case creationData creation of
--             (BoxData x) => ?sdklfmksdm
--             (WallData x) => ?sdflkgmsdk_2
--             (InvisibleWallData x) => noFixtures [defaultWallFixture x]


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
%name Object object

Show Object where
  show (MkObject id name physicsProperties renderDescription tags scripts control health)
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

-- TODO invisible e.g. should return aabb
dimensions : Object -> Vector2D
dimensions object = case renderDescription object of
  DrawBox ref dim => dim
  TileWith ref (x, y) (nx, ny) => (cast nx * x, cast ny * y)
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

-- TODO fix
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

-- w : Object -> Double
-- w = fst . dimensions
--
-- h : Object -> Double
-- h = snd . dimensions

x : Object -> Double
x = fst . position

y : Object -> Double
y = snd . position

controlFromDescriptor : ObjectDescriptor -> Maybe ObjectControl
controlFromDescriptor = map (MkObjectControl noControl) . control

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
  let noFixtures = MkPhysicsProperties (type bdesc) (position creation)
                                       nullVector (angle creation) (-1) empty
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
                  (controlFromDescriptor desc) health


fromDescriptorCreation' : ObjectDescriptor -> Creation ->
                          STrans m (Maybe Object) xs (const xs)
fromDescriptorCreation' desc creation = pure $ fromDescriptorCreation desc creation

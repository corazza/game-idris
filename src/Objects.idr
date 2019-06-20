module Objects

import Graphics.SDL2
-- import Data.AVL.Dict
import Data.AVL.Set
import Physics.Vector2D
import Control.ST

import Descriptors
import Resources
import Common
import Script

import Data.AVL.DDict

%access public export

data CompleteRenderDescriptor
  = DrawBox ResourceReference
  | TileWith ResourceReference Vector2D (Nat, Nat)
  | Invisible

decideRenderDescription : IncompleteRenderDescriptor -> CreationData ->
                          Maybe CompleteRenderDescriptor
decideRenderDescription Invisible cdata = Just Invisible
decideRenderDescription (DrawBox x) (BoxData y) = Just $ DrawBox x
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

record PhysicsProperties where
  constructor MkPhysicsProperties
  position : Vector2D
  velocity : Vector2D
  dimensions : Vector2D
  angle : Double
  density : Double
  friction : Double
  mass : Double -- overwritten on Scene.addWithId
  type : BodyType
  touching : Set ObjectId

fromBodyDescriptor : (bodyDescription : BodyDescriptor) ->
                     (dimensions : Vector2D) ->
                     (angle : Double) ->
                     (position : Vector2D) ->
                     PhysicsProperties
fromBodyDescriptor bodyDescription dimensions angle position
  = MkPhysicsProperties position nullVector dimensions angle
                        (density bodyDescription) (friction bodyDescription)
                        -- mass overwritten on addBody
                        (-1) (type bodyDescription) empty

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

-- all changes -> physics, physics -> objects
record Object where
  constructor MkObject
  id : String
  name : String
  physicsProperties : PhysicsProperties
  controlState : ControlState
  renderDescription : CompleteRenderDescriptor
  tags : Set ObjectTag
  health : Maybe Health -- TODO move health, controlState, and tags into components
  control : Maybe ControlDescriptor
  scripts : Scripts
%name Object object

Show Object where
  show (MkObject id name physicsProperties controlState renderDescription tags
                 health control scripts)
    =    "{ object | "
      ++   "id: " ++ id
      ++ ", name: " ++ name
      ++ ", controlState: " ++ show controlState
      ++ ", tags: " ++ show tags
      ++ ", health: " ++ show health
      ++ " }"

Damagable Object where
  takeDamage x = record { health $= map (takeDamage x) }
  alive object = case health object of
    Nothing => True
    Just health => alive health

deactivateCollision : (name : String) -> Object -> Object
deactivateCollision name = record { scripts->collisions $= delete name }

physicsUpdate : (PhysicsProperties -> PhysicsProperties) -> Object -> Object
physicsUpdate f = record { physicsProperties $= f }

jumping : Object -> Bool
jumping = jumping . controlState

resetControl : Object -> Object
resetControl object = record { controlState $=
  record { canJump = not (jumping object) } } object

touching : Object -> Set ObjectId
touching = touching . physicsProperties

addTouching : ObjectId -> Object -> Object
addTouching id = physicsUpdate $ record { touching $= insert id }

removeTouching : ObjectId -> Object -> Object
removeTouching id = let to_remove = insert id empty in
  physicsUpdate $ record { touching $= \t => difference t to_remove }

getControlST : Object -> STrans m (Maybe ControlDescriptor) xs (const xs)
getControlST = pure . control

density : Object -> Double
density = density . physicsProperties

friction : Object -> Double
friction = friction . physicsProperties

angle : Object -> Double
angle = angle . physicsProperties

mass : Object -> Double
mass = mass . physicsProperties

dimensions : Object -> Vector2D
dimensions = dimensions . physicsProperties

dim : Object -> Vector2D
dim = dimensions

position : Object -> Vector2D
position = position . physicsProperties

velocity : Object -> Vector2D
velocity = velocity . physicsProperties

w : Object -> Double
w = fst . dimensions

h : Object -> Double
h = snd . dimensions

x : Object -> Double
x = fst . position

y : Object -> Double
y = snd . position

fromDescriptorCreation : ObjectDescriptor -> Creation -> Maybe Object
fromDescriptorCreation desc creation = with Maybe do
  let id = getOrDefault "" (id creation)
  let tags = tags creation `union` tags desc
  let health = map fromFull (health desc)
  let scripts = decideScripts desc creation
  let irdesc = renderDescription desc
  let bdesc = bodyDescription desc
  let cdata = creationData creation
  dimensions <- decideDimensions irdesc cdata bdesc
  let physics_properties =
    fromBodyDescriptor bdesc dimensions (angle creation) (position creation)
  crdesc <- decideRenderDescription irdesc cdata
  pure $ MkObject id (name desc) physics_properties noControl crdesc tags health
                  (control desc) scripts

fromDescriptorCreation' : ObjectDescriptor -> Creation ->
                          STrans m (Maybe Object) xs (const xs)
fromDescriptorCreation' desc creation = pure $ fromDescriptorCreation desc creation

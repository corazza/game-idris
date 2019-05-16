module Objects

import Graphics.SDL2
import Data.AVL.Dict
import Data.AVL.Set
import Physics.Vector2D

import Descriptors
import Resources
import Common
import Script

%access public export

data CompleteRenderDescriptor
  = DrawBox ResourceReference
  | TileWith ResourceReference Vector2D (Nat, Nat)
  | Invisible

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

record Scripts where
  constructor MkScripts
  attack : Maybe (ActionParameters -> UnitScript)
  collisions : List (CollisionData -> UnitScript)

noScripts : Scripts
noScripts = MkScripts Nothing empty

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

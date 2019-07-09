module Scene.Util

import Data.AVL.Dict
import Data.AVL.DDict

import Physics.Box2D
import Physics.Vector2D
import Common
import Objects
import Events
import Descriptors
import GameIO
import AI.PAI

%access public export

Entry : Type
Entry = (Object, Body)

SceneObjects : Type
SceneObjects = DDict ObjectId Entry

noObjects : SceneObjects
noObjects = empty

getObject' : (id : ObjectId) -> (dict : SceneObjects) -> Maybe Object
getObject' id = map fst . lookup id

getObjects' : (dict : SceneObjects) -> List Object
getObjects' = map fst . values

addObjectBody : Entry -> (dict : SceneObjects) -> SceneObjects
addObjectBody entry@(object, body) = insert (id object) entry

getBody' : (id : ObjectId) -> (dict : SceneObjects) -> Maybe Body
getBody' id = map snd . lookup id

objectUpdate : (f : Object -> Object) -> (entry : Entry) -> Entry
objectUpdate f (object, body) = (f object, body)

SceneEvents : Type
SceneEvents = List Events.Event

noEvents : SceneEvents
noEvents = empty

addBody' : (GameIO m, Monad m) => (world : Box2D.World) -> Object -> m (Int, Body)
addBody' world object = with m do
  let def = MkBodyDefinition
    (type object) (position object) (Just (angle object)) (fixedRotation object) (bullet object)
  (id, body) <- createBody world def
  traverse (createFixture body) (fixtures $ physicsProperties object)
  pure (id, body)

updateFromBody : (GameIO m, Monad m) => Entry -> m Entry
updateFromBody (object, body) = with m do
    newPosition <- getPosition body
    newAngle <- getAngle body
    newVelocity <- getVelocity body
    let object' = physicsUpdate (record {
                    position   = newPosition,
                      angle    = newAngle,
                      velocity = newVelocity}) object
    pure $ (object', body)

commitControl : (GameIO m, Monad m) => (world : Box2D.World) -> List Entry -> m ()
commitControl world [] = pure ()
commitControl world ((object, body) :: xs) = with m do
  applyImpulse body (movementImpulse object)
  commitControl world xs

commitEffect : GameIO m => Monad m => (world : Box2D.World) -> Entry -> PhysicsEffect -> m ()
-- commitEffect w e eff = pure ()
commitEffect world (object, body) (Drag factor offset) = with m do
  velocity <- getVelocity body
  let norm' = norm velocity
  if (norm' > 0.001)
    then with m do
      let direction = negate $ normed velocity
      let force = (0.5*factor*norm'*norm') `scale` direction
      applyForce body force offset
      pure ()
    else pure ()

commitEffects : (GameIO m, Monad m) => (world : Box2D.World) -> List Entry -> m ()
commitEffects world [] = pure ()
commitEffects world (x@(object, body) :: xs) = with m do
  sequence_ $ map (commitEffect world x) (effects object)
  commitEffects world xs

record PScene where
  constructor MkPScene
  idCounter : Nat
  objects : SceneObjects
  events : SceneEvents
  physicsIds : Dict Int String
  -- timeCounter : Int
  background : Background
  dimensions : Vector2D

emptyPScene : Background -> Vector2D -> PScene
emptyPScene = MkPScene Z noObjects noEvents empty

-- updateCount : (ticks : Int) -> PScene -> PScene
-- updateCount ticks = record {timeCounter $= (+ ticks)}

getOutside : (dimensions : Vector2D) -> List Object -> List ObjectId
getOutside (w, h) [] = []
getOutside dimensions@(w, h) (object :: xs) = let (x, y) = position object in
  if x > w || x < -w || y > h || y < -h
    then id object :: getOutside dimensions xs
    else getOutside dimensions xs

getInfo : ObjectId -> SceneObjects -> Maybe ObjectInfo
getInfo dict = map (toInfo . fst) . lookup dict

objectInfoUpdates : SceneObjects -> List ObjectId -> List ObjectInfo
objectInfoUpdates dict = catMaybes . foldr (\x, xs => (getInfo x dict :: xs)) []

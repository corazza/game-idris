module Scene.Util

import Data.AVL.Dict
import Data.AVL.DDict

import Physics.Box2D
import Physics.Vector2D
import Common
import Objects
import Events
import Descriptors

%access public export

SceneObjects : Type
SceneObjects = DDict ObjectId (Object, Body)

noObjects : SceneObjects
noObjects = empty

getObject' : (id : ObjectId) -> (dict : SceneObjects) -> Maybe Object
getObject' id = map fst . lookup id

getObjects' : (dict : SceneObjects) -> List Object
getObjects' = map fst . values

addObjectBody : (Object, Body) -> (dict : SceneObjects) -> SceneObjects
addObjectBody entry@(object, body) = insert (id object) entry

getBody' : (id : ObjectId) -> (dict : SceneObjects) -> Maybe Body
getBody' id = map snd . lookup id

objectUpdate : (f : Object -> Object) -> (entry : (Object, Body)) -> (Object, Body)
objectUpdate f (object, body) = (f object, body)

SceneEvents : Type
SceneEvents = List Events.Event

noEvents : SceneEvents
noEvents = empty

updateFromBody : (Monad m, Box2DPhysics m) => (Object, Body) -> m (Object, Body)
updateFromBody (object, body) = do
    newPosition <- getPosition body -- idk why !(getPosition body) doesn't work in record
    newAngle <- getAngle body
    newVelocity <- getVelocity body
    let object' = physicsUpdate (record {
                    position   = newPosition,
                      angle    = newAngle,
                      velocity = newVelocity}) object
    pure $ (object', body)

record PScene where
  constructor MkPScene
  idCounter : Nat
  objects : SceneObjects
  events : SceneEvents
  physicsIds : Dict Int String
  background : Background

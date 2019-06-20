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

Entry : Type -- not sure why this isn't just Maybe
Entry = Either () (Object, Body)

SceneObjects : Type
SceneObjects = Dict ObjectId Entry

-- TODO remove duplication via fst and snd
getObject' : (id : ObjectId) -> (dict : SceneObjects) -> Maybe Object
getObject' id dict = case lookup id dict of
  Just (Right (object, body)) => Just object
  _ => Nothing

extractObject : Entry -> Maybe Object
extractObject (Left l) = Nothing
extractObject (Right (object, body)) = Just object

extractBoth : Entry -> Maybe (Object, Body)
extractBoth (Left l) = Nothing
extractBoth (Right r) = Just r

getObjects' : (dict : SceneObjects) -> List Object
getObjects' dict = catMaybes $ map extractObject (values dict) -- let entries = values dict in ?sdfjn

-- TODO shorten with map
getBody' : (id : ObjectId) -> (dict : SceneObjects) -> Maybe Body
getBody' id dict = case lookup id dict of
  Just (Right (object, body)) => Just body
  _ => Nothing

getObjectBodys : (dict : SceneObjects) -> List (Object, Body)
getObjectBodys dict = catMaybes $ map extractBoth (values dict)

entryUpdate : (f : Object -> Object) -> Entry -> Entry
entryUpdate f (Right (object, body)) = Right (f object, body)
entryUpdate f x = x

objectUpdate : (id : ObjectId) -> (f : Object -> Object) -> (dict : SceneObjects) -> SceneObjects
objectUpdate id f = update id (entryUpdate f)

removeEntry : (id : ObjectId) -> (dict : SceneObjects) -> SceneObjects
removeEntry id = update id (const $ Left ())

SceneEvents : Type
SceneEvents = List Events.Event

updateFromBody : (Monad m, Box2DPhysics m) => Entry -> m Entry
updateFromBody (Left l) = pure $ Left ()
updateFromBody (Right (object, body)) = do
  newPosition <- getPosition body -- idk why !(getPosition body) doesn't work in record
  newAngle <- getAngle body
  newVelocity <- getVelocity body
  let object' = physicsUpdate (record {
                  position   = newPosition,
                    angle    = newAngle,
                    velocity = newVelocity}) object
  pure $ Right (object', body)

record PScene where
  constructor MkPScene
  idCounter : Nat
  objects : SceneObjects
  events : SceneEvents
  physicsIds : Dict Int String
  background : Background

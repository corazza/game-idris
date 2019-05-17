module Scene.Util

import Data.AVL.Dict

import Physics.Box2D
import Common
import Objects
import Events

%access public export

Entry : Type
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

getBoths : (dict : SceneObjects) -> List (Object, Body)
getBoths dict = catMaybes $ map extractBoth (values dict)

entryUpdate : (f : Object -> Object) -> Entry -> Entry
entryUpdate f (Right (object, body)) = Right (f object, body)
entryUpdate f x = x

objectUpdate : (id : ObjectId) -> (f : Object -> Object) -> (dict : SceneObjects) -> SceneObjects
objectUpdate id f = update id (entryUpdate f)

removeEntry : (id : ObjectId) -> (dict : SceneObjects) -> SceneObjects
removeEntry id = update id (const $ Left ())

SceneEvents : Type
SceneEvents = List Events.Event

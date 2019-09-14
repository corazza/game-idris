module Timeline

import Physics.Box2D

import GameIO
import Objects
import Exception
import Timeline.Items
import Descriptions.ItemDescription

public export
record Character where
  constructor MkCharacter
  name : String
  ref : ContentReference
  position : Vector2D
  map : ContentReference
  items : Items
%name Character character

export
Show Character where
  show (MkCharacter name object position map items)
    = "{ name: " ++ name ++ ", object: " ++ object ++ ", position: " ++ show position
    ++ ", map: " ++ map ++ " }"

ObjectCaster Character where
  objectCast dict = with Checked do
    name <- getString "name" dict
    object <- getString "object" dict
    position <- getVector "position" dict
    map <- getString "map" dict
    items <- the (Checked Items) $ getCastable "items" dict
    pure $ MkCharacter name object position map items

Serialize Character where
  toDict character = with ST do
    characterObject <- makeObject
    addString characterObject "name" $ name character
    addString characterObject "object" $ ref character
    addVector characterObject "position" $ position character
    addString characterObject "map" $ map character
    itemsObject <- toDict $ items character
    addObject characterObject "items" itemsObject
    getDict characterObject

export
setMap : ContentReference -> Character -> Character
setMap map' = record { map = map' }

public export
record Timeline where
  constructor MkTimeline
  character : CharacterId -- default
  characters : Dict CharacterId Character
%name Timeline timeline

export
Show Timeline where
  show (MkTimeline character characters)
    = "{ character: " ++ character ++ ", characters: " ++ show characters ++ " }"

toCharacter : (String, JSON) -> Checked (String, Character)
toCharacter (id, json) = case the (Checked Character) (cast json) of
  Left e => fail e
  Right character => pure (id, character)

getCharacters : Dict String JSON -> Checked (Dict String Character)
getCharacters dict = case lookup "characters" dict of
  Nothing => fail "missing characters list"
  Just (JObject xs) => with Checked do
    let attempt = map toCharacter xs
    characters <- catResults attempt
    pure $ fromList characters
  _ => fail "characters aren't JObject"

export
ObjectCaster Timeline where
  objectCast dict = with Checked do
    character <- getString "character" dict
    getCharacters dict >>= pure . MkTimeline character

export
Serialize Timeline where
  toDict timeline = with ST do
    timelineObject <- makeObject
    addString timelineObject "character" $ character timeline
    addObject timelineObject "characters" $ makeDict $ characters timeline
    getDict timelineObject

export
loadTimeline : GameIO m => String -> m (Checked Timeline)
loadTimeline filepath = with m do
  Just json <- loadJSON filepath
          | Nothing => pure (fail $ "couldn't load timeline " ++ filepath)
  pure $ cast json

export
saveTimeline : GameIO m => (filepath : String) -> Timeline -> m ()
saveTimeline filepath timeline = write (pretty timeline) filepath

export
getCharacter : CharacterId -> Timeline -> Maybe Character
getCharacter id = lookup id . characters

export
updateCharacter : CharacterId -> (f : Character -> Character) -> Timeline -> Timeline
updateCharacter id f timeline = record { characters $= update id f } timeline

export
itemsToCharacter : (f : Items -> Items) -> (Character -> Character)
itemsToCharacter f = record { items $= f }

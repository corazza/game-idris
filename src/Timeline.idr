module Timeline

import Physics.Box2D

import GameIO
import Objects
import Exception

public export
record Character where
  constructor MkCharacter
  name : String
  ref : ContentReference
  position : Vector2D
  map : ContentReference
%name Character character

export
Show Character where
  show (MkCharacter name object position map)
    = "{ name: " ++ name ++ ", object: " ++ object ++ ", position: " ++ show position
    ++ ", map: " ++ map ++ " }"

ObjectCaster Character where
  objectCast dict = with Checked do
    name <- getString "name" dict
    object <- getString "object" dict
    position <- getVector "position" dict
    map <- getString "map" dict
    pure $ MkCharacter name object position map

public export
record Timeline where
  constructor MkTimeline
  character : String -- default
  characters : Dict String Character
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
loadTimeline : GameIO m => String -> m (Checked Timeline)
loadTimeline filepath = with m do
  Just json <- loadJSON filepath
          | Nothing => pure (fail $ "couldn't load timeline " ++ filepath)
  pure $ cast json

module Timeline

import Physics.Vector2D
import GameIO
import Resources
import Common
import Exception

public export
record Character where
  constructor MkCharacter
  name : String
  object : ResourceReference
  position : Vector2D
  map : ResourceReference
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
  character : String
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
    characters <- foldr toChecked (pure empty) attempt
    pure $ fromList characters
  _ => fail "characters aren't JObject"

ObjectCaster Timeline where
  objectCast dict = with Checked do
    character <- getString "character" dict
    getCharacters dict >>= pure . MkTimeline character

public export
GameIO m => SimpleLoader m Timeline where
  load id = checkedJSONLoad ("saves/" ++ id ++ ".json")

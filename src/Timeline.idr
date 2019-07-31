module Timeline

import Physics.Box2D

import GameIO
import Objects
import Exception

public export
record Equipment where
  constructor MkEquipment
  head : Maybe ContentReference
  hands : Maybe ContentReference
  feet : Maybe ContentReference

ObjectCaster Equipment where
  objectCast dict = with Checked do
    head <- getStringMaybe "head" dict
    hands <- getStringMaybe "hands" dict
    feet <- getStringMaybe "feet" dict
    pure $ MkEquipment head hands feet

Serialize Equipment where
  toDict eq = with ST do
    equipmentObject <- makeObject
    addStringMaybe equipmentObject "head" $ head eq
    addStringMaybe equipmentObject "hands" $ hands eq
    addStringMaybe equipmentObject "feet" $ feet eq
    getDict equipmentObject

export
noEquipment : Equipment
noEquipment = MkEquipment Nothing Nothing Nothing

public export
Inventory : Type
Inventory = Dict ContentReference Nat
%name Inventory inventory

discardZ : Inventory -> Inventory
discardZ = fromList . filter keepEntry . toList where
  keepEntry : (ContentReference, Nat) -> Bool
  keepEntry = (>Z) . snd

export
items : Inventory -> List (ContentReference, Nat)
items = toList . discardZ

addItem : ContentReference -> Inventory -> Inventory
addItem ref inventory = case hasKey ref inventory of
  False => insert ref 1 inventory
  True => update ref S inventory

natToJSON : Nat -> JSON
natToJSON = JNumber . cast

inventoryToJSONDict : Inventory -> JSONDict
inventoryToJSONDict = map natToJSON . discardZ -- fromList . map toJSONEntry . toList

toInventoryEntry : (String, JSON) -> Checked (String, Nat)
toInventoryEntry (item_ref, JNumber x) = pure (item_ref, toNat $ the Int $ cast x)
toInventoryEntry (item_ref, _) = fail $ "field " ++ item_ref ++ " must be JNumber (Nat)"

getInventory : JSONDict -> Checked Inventory
getInventory dict = case lookup "inventory" dict of
  Nothing => fail "no inventory"
  Just (JObject xs) => catResults (map toInventoryEntry xs) >>= pure . fromList
  _ => fail "inventory must be JObject"

public export
data Items = MkItems Equipment Inventory

ObjectCaster Items where
  objectCast dict = with Checked do
    equipment <- the (Checked Equipment) $ getCastable "equipment" dict
    inventory <- getInventory dict
    pure $ MkItems equipment inventory

Serialize Items where
  toDict (MkItems equipment inventory) = with ST do
    let inventoryObject = inventoryToJSONDict inventory
    equipmentObject <- toDict equipment
    itemsObject <- makeObject
    addObject itemsObject "inventory" inventoryObject
    addObject itemsObject "equipment" equipmentObject
    getDict itemsObject

addToInventory : ContentReference -> Items -> Items
addToInventory ref items@(MkItems equipment inventory)
  = MkItems equipment (addItem ref inventory)

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
attackItem : Character -> Maybe ContentReference
attackItem character = let (MkItems equipment inventory) = items character
                           in hands equipment

export
setMap : ContentReference -> Character -> Character
setMap map' = record { map = map' }

export
loot : ContentReference -> Character -> Character
loot ref = record { items $= addToInventory ref }

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

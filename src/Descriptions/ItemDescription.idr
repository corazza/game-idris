module Descriptions.ItemDescription

import GameIO
import Exception
import Objects
import Descriptions.AbilityDescription

public export
data EquipSlot
  = Head
  | Hands
  | Legs

export
Show EquipSlot where
  show Head = "head"
  show Hands = "hands"
  show Legs = "legs"

Cast String (Checked EquipSlot) where
  cast "head" = pure Head
  cast "hands" = pure Hands
  cast "legs" = pure Legs
  cast _ = fail "equip slot must be of \"head\" | \"hands\" | \"legs\""

getSlot : JSONDict -> Checked EquipSlot
getSlot dict = case lookup "slot" dict of
  Just (JString x) => cast x
  Nothing => fail "slot field missing"
  _ => fail "slot must be string"

public export
record EquipDescription where
  constructor MkEquipDescription
  slot : EquipSlot
  ability : AbilityDescription

ObjectCaster EquipDescription where
  objectCast dict = with Checked do
    slot <- getSlot dict
    ability <- the (Checked AbilityDescription) $ getCastable "ability" dict
    pure $ MkEquipDescription slot ability

public export
record ItemDescription where
  constructor MkItemDescription
  name : String
  unique : Bool
  equip : Maybe EquipDescription
  icon : ContentReference

export
ObjectCaster ItemDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    unique <- getBoolMaybe "unique" dict
    equip <- the (Checked (Maybe EquipDescription)) $ getCastableMaybe "equip" dict
    icon <- getString "icon" dict
    pure $ MkItemDescription name (fromMaybe False unique) equip icon

module Descriptions.ItemDescription

import Physics.Vector2D

import GameIO
import Exception
import Objects
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.RenderDescription

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
  ability : Maybe AbilityDescription
  offset : Vector2D

ObjectCaster EquipDescription where
  objectCast dict = with Checked do
    slot <- getSlot dict
    ability <- the (Checked (Maybe AbilityDescription)) $
      getCastableMaybe "ability" dict
    offset <- getVectorOrDefault nullVector "offset" dict
    pure $ MkEquipDescription slot ability offset

public export
record ItemDescription where
  constructor MkItemDescription
  name : String
  unique : Bool
  equip : Maybe EquipDescription
  icon : ContentReference
  attackRender : Maybe RenderMethod

export
ObjectCaster ItemDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    unique <- getBoolMaybe "unique" dict
    equip <- the (Checked (Maybe EquipDescription)) $ getCastableMaybe "equip" dict
    icon <- getString "icon" dict
    attackRender <- the (Checked (Maybe RenderMethod)) $
      getCastableMaybe "attackRender" dict
    pure $ MkItemDescription name (fromMaybe False unique) equip icon attackRender

export
item_offset : ItemDescription -> Vector2D
item_offset desc = case equip desc of
  Nothing => nullVector
  Just x => offset x

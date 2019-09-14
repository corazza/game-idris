module Timeline.Items

import Objects
import Exception
import GameIO
import Descriptions.ItemDescription

public export
record Equipment where
  constructor MkEquipment
  head : Maybe ContentReference
  hands : Maybe ContentReference
  legs : Maybe ContentReference
%name Equipment equipment

ObjectCaster Equipment where
  objectCast dict = with Checked do
    head <- getStringMaybe "head" dict
    hands <- getStringMaybe "hands" dict
    legs <- getStringMaybe "legs" dict
    pure $ MkEquipment head hands legs

Serialize Equipment where
  toDict eq = with ST do
    equipmentObject <- makeObject
    addStringMaybe equipmentObject "head" $ head eq
    addStringMaybe equipmentObject "hands" $ hands eq
    addStringMaybe equipmentObject "legs" $ legs eq
    getDict equipmentObject

export
noEquipment : Equipment
noEquipment = MkEquipment Nothing Nothing Nothing

export
addItemEquipment : ContentReference -> EquipSlot -> Equipment -> Equipment
addItemEquipment ref Head = record { head = Just ref }
addItemEquipment ref Hands = record { hands = Just ref }
addItemEquipment ref Legs = record { legs = Just ref }

export
getAtSlotEquipment : Equipment -> EquipSlot -> Maybe ContentReference
getAtSlotEquipment equipment Head = head equipment
getAtSlotEquipment equipment Hands = hands equipment
getAtSlotEquipment equipment Legs = legs equipment

export
resetSlotEquipment : EquipSlot -> Equipment -> Equipment
resetSlotEquipment Head = record { head = Nothing }
resetSlotEquipment Hands = record { hands = Nothing }
resetSlotEquipment Legs = record { legs = Nothing }

export
inSlot : EquipSlot -> Maybe ContentReference -> ContentReference -> Maybe EquipSlot
inSlot slot Nothing ref = Nothing
inSlot slot (Just x) ref = case x == ref of
  False => Nothing
  True => Just slot

export
inEquipment : ContentReference -> Equipment -> Maybe EquipSlot
inEquipment ref (MkEquipment head hands legs) = head' $ catMaybes $
  [inSlot Head head ref, inSlot Hands hands ref, inSlot Legs legs ref]

public export
Inventory : Type
Inventory = Dict ContentReference Nat
%name Inventory inventory

export
discardZ : Inventory -> Inventory
discardZ = fromList . filter keepEntry . toList where
  keepEntry : (ContentReference, Nat) -> Bool
  keepEntry = (>Z) . snd

export
removeFromInventory : ContentReference -> Inventory -> Inventory
removeFromInventory ref = discardZ . fromList . map removeEntry . toList where
  removeEntry : (ContentReference, Nat) -> (ContentReference, Nat)
  removeEntry (ref', S k) = if ref' == ref then (ref', k) else (ref', S k)
  removeEntry (ref', Z) = (ref', Z)

export
items : Inventory -> List (ContentReference, Nat)
items = toList . discardZ

export
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

export
ObjectCaster Items where
  objectCast dict = with Checked do
    equipment <- the (Checked Equipment) $ getCastable "equipment" dict
    inventory <- getInventory dict
    pure $ MkItems equipment inventory

export
Serialize Items where
  toDict (MkItems equipment inventory) = with ST do
    let inventoryObject = inventoryToJSONDict inventory
    equipmentObject <- toDict equipment
    itemsObject <- makeObject
    addObject itemsObject "inventory" inventoryObject
    addObject itemsObject "equipment" equipmentObject
    getDict itemsObject

export
addToInventory : ContentReference -> Items -> Items
addToInventory ref (MkItems equipment inventory)
  = MkItems equipment (addItem ref inventory)

export
addToEquipment : ContentReference -> EquipSlot -> Items -> Items
addToEquipment ref slot (MkItems equipment inventory)
  = MkItems (addItemEquipment ref slot equipment) inventory

export
attackItem : Items -> Maybe ContentReference
attackItem (MkItems equipment inventory) =  hands equipment

export
resetSlot : EquipSlot -> Items -> Items
resetSlot slot (MkItems equipment inventory)
  = let newEquipment = resetSlotEquipment slot equipment
        in MkItems newEquipment inventory

export
removeItem : ContentReference -> Items -> Items
removeItem ref (MkItems equipment inventory)
  = let newInventory = removeFromInventory ref inventory
        in MkItems equipment newInventory

export
hasEquipped : ContentReference -> Items -> Maybe EquipSlot
hasEquipped ref (MkItems equipment inventory) = inEquipment ref equipment

export
hasItem : ContentReference -> Items -> Bool
hasItem ref (MkItems equipment inventory) = case lookup ref inventory of
  Just (S k) => True
  _ => False

export
getAtSlot : EquipSlot -> Items -> Maybe ContentReference
getAtSlot slot (MkItems equipment inventory) = getAtSlotEquipment equipment slot

export
loot : ContentReference -> Items -> Items
loot = addToInventory

export
equip : ContentReference -> EquipSlot -> Items -> Items
equip = addToEquipment

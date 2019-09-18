module Server.Rules.Scripts.AttackScripts

import Server.Rules.RuleScript
import Server.Rules.RulesOutput
import Server.Rules.Scripts.BehaviorScripts
import Client.ClientCommands
import Descriptions.AbilityDescription
import Descriptions.ItemDescription
import Timeline.Items
import Objects
import Exception

export
clearSlot : (equipper : ObjectId) -> EquipSlot -> UnitRuleScript
clearSlot equipper slot = with RuleScript do
  Just (Just item_ref) <- QueryItems equipper (getAtSlot slot) | pure ()
  UpdateItems equipper $ resetSlot slot
  lootScript equipper item_ref

export
equipScript : (equipper : ObjectId) ->
              (item : ContentReference) ->
              UnitRuleScript
equipScript equipper item = case !(QueryItems equipper (hasItem item)) of
  Just True => with RuleScript do
    Right item_desc <- GetItemDescription item
          | Left e => Log ("couldn't get item " ++ item ++ ", error: " ++ e)
    case equip item_desc of
      Nothing => pure ()
      Just equip_desc => with RuleScript do
        let slot' = slot equip_desc
        clearSlot equipper slot'
        UpdateItems equipper $ equip item slot'
        UpdateItems equipper $ removeItem item
        RulesClientCommand equipper RefreshInventory
  _ => pure ()

export
unequipScript : (equipper : ObjectId) ->
                (item : ContentReference) ->
                UnitRuleScript
unequipScript equipper item = case !(QueryItems equipper (hasEquipped item)) of
  Just (Just slot) => clearSlot equipper slot
  _ => pure ()

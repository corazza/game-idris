module Client.UI.Character

import GameIO
import Exception
import JSONCache
import Timeline
import Objects
import Descriptions.Color
import Descriptions.SurfaceDescription
import Descriptions.ItemDescription
import Client.UI.PUI

makeInventoryClick : ContentReference -> Maybe Click
makeInventoryClick = Just . Inventory

makeCharacterClick : ContentReference -> Maybe Click
makeCharacterClick = Just . Character

characterItemSurface : (click : ContentReference) ->
                       (icon : ContentReference) ->
                       (name : String) ->
                       SurfaceDescription
characterItemSurface click icon name
  = let inactive = Image icon
        render_desc = MkSurfaceRenderDescription inactive Nothing Nothing
        click' = makeCharacterClick click
        sparams = MkSurfaceParameters
          (Just (50, 50)) (Just render_desc) click' Center Vertical
        in MkSurfaceDescription Nothing (Just "image") sparams empty

characterItemSurfaceEmpty : SurfaceDescription
characterItemSurfaceEmpty
  = let sparams = MkSurfaceParameters
          (Just (50, 50)) Nothing Nothing Center Vertical
        in MkSurfaceDescription Nothing (Just "image") sparams empty

characterEquipmentRef : SurfaceName -> SurfaceReference
characterEquipmentRef name
  = MkSurfaceReference "main/ui/character.json" ["slots", name, "image"]

equipmentSurface : PreloadResults ->
                   Maybe ContentReference ->
                   (slot : String) ->
                   Checked (SurfaceReference, SurfaceDescription)
equipmentSurface preload Nothing slot
  = pure (characterEquipmentRef slot, characterItemSurfaceEmpty)
equipmentSurface preload (Just item_ref) slot = with Checked do
  item_desc <- getItemDescription item_ref preload
  let icon = icon item_desc
  let name = name item_desc
  let ref = characterEquipmentRef slot
  pure $ (ref, characterItemSurface item_ref icon name)

export
equipmentSurfaces : Items ->
                    PreloadResults ->
                    Checked (List (SurfaceReference, SurfaceDescription))
equipmentSurfaces (MkItems equipment inventory) preload = with Checked do
  head_desc <- equipmentSurface preload (head equipment) "head"
  hands_desc <- equipmentSurface preload (hands equipment) "hands"
  legs_desc <- equipmentSurface preload (legs equipment) "legs"
  pure $ [head_desc, hands_desc, legs_desc]

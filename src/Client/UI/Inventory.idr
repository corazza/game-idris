module Client.UI.Inventory

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

numberSurface : (n : Nat) -> Maybe Click -> SurfaceDescription
numberSurface n click
  = let inactive = Text (show n) "main/fonts/inventory_count.json"
        render_desc = MkSurfaceRenderDescription inactive Nothing Nothing
        sparams = MkSurfaceParameters
          (Just (15, 15)) (Just render_desc) click Center Vertical
        in MkSurfaceDescription Nothing Nothing sparams empty

inventoryItemSurface : (click : ContentReference) ->
                       (icon : ContentReference) ->
                       (name : String) ->
                       (n : Nat) ->
                       SurfaceDescription
inventoryItemSurface click icon name n
  = let inactive = Image icon
        render_desc = MkSurfaceRenderDescription inactive Nothing Nothing
        click' = makeInventoryClick click
        sparams = MkSurfaceParameters
          (Just (50, 50)) (Just render_desc) click' Center Vertical
        num = numberSurface n click'
        in MkSurfaceDescription Nothing Nothing sparams [num]

partialListApply : List (a -> b) -> List a -> List b
partialListApply = zipWith (\p, q => p q)

export
inventorySurfaces : Items -> PreloadResults -> Checked (List SurfaceDescription)
inventorySurfaces (MkItems equipment inventory) preload = with Checked do
  let items' = items inventory
  let item_refs = map fst items'
  let item_nums = map snd items'
  item_descs <- catResults $ map (flip getItemDescription preload) item_refs
  let icon_refs = map icon item_descs
  let item_names = map name item_descs
  let surface_clicks = map inventoryItemSurface item_refs
  let surface_icons = partialListApply surface_clicks icon_refs
  let surface_names = partialListApply surface_icons item_names
  let surfaces = partialListApply surface_names item_nums
  pure $ surfaces

export
itemlistRef : SurfaceReference
itemlistRef = MkSurfaceReference "main/ui/inventory.json" ["itemlist"]

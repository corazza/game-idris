module Creator.UI.ObjectSurfaces

import GameIO
import Exception
import JSONCache
import Timeline.Items
import Objects
import Descriptions.Color
import Descriptions.SurfaceDescription
import Descriptions.ObjectDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ItemDescription
import Client.UI.PUI

iconSurface : PreloadResults -> ObjectDescription -> SurfaceDescription
iconSurface preload desc
  = let iconMaybe = join $ map (eitherToMaybe . getIcon preload) $ render desc
        inactive = Image $ fromMaybe "main/images/wooden_crate.png" iconMaybe
        render_desc = MkSurfaceRenderDescription inactive Nothing Nothing
        sparams = MkSurfaceParameters
          (Just (50, 50)) (Just render_desc) Nothing Center Vertical
        in MkSurfaceDescription Nothing Nothing sparams empty

textSurface : ObjectDescription -> SurfaceDescription
textSurface desc
  = let inactive = Text (name desc) "main/fonts/inventory_count.json"
        render_desc = MkSurfaceRenderDescription inactive Nothing Nothing
        sparams = MkSurfaceParameters
          (Just (50, 20)) (Just render_desc) Nothing Center Vertical
        in MkSurfaceDescription Nothing Nothing sparams empty

objectSurface : PreloadResults -> (ContentReference, ObjectDescription) -> SurfaceDescription
objectSurface preload (ref, desc)
  = let iconSurface' = iconSurface preload desc
        textSurface' = textSurface desc
        sparams = MkSurfaceParameters
          Nothing Nothing (Just $ CreatorObjectSelect ref) Center Vertical
        in MkSurfaceDescription Nothing Nothing sparams [iconSurface', textSurface']

export
objectsSurfaces : PreloadResults -> List SurfaceDescription
objectsSurfaces preload = map (objectSurface preload) $ findAll $ creatorObjects preload
  where
    findOne : ContentReference -> Checked (ContentReference, ObjectDescription)
    findOne ref = getObjectDescription ref preload >>= \x => pure (ref, x)

    findAll : List ContentReference -> List (ContentReference, ObjectDescription)
    findAll = fromMaybe empty . eitherToMaybe . catResults . map findOne

export
objectslistRef : SurfaceReference
objectslistRef = MkSurfaceReference "main/ui/creator/objects.json" ["objectslist"]

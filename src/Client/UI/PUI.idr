module Client.UI.PUI

import GameIO
import Descriptions.SurfaceDescription
import JSONCache
import Objects

public export
data SurfaceState = Inactive | Hover | Clicked

export
Show SurfaceState where
  show Inactive = "inactive"
  show Hover = "hover"
  show Clicked = "clicked"

-- record SurfaceParameters where
--   constructor MkSurfaceParameters
--   dimensions : Maybe (Int, Int)
--   render : SurfaceRenderDescription
--   click : Maybe String
--   displayStyle : DisplayStyle
--   layout : Layout

public export
record UISurface where
  constructor MkUISurface
  id : SurfaceId
  surfaceParameters : SurfaceParameters
  state : SurfaceState
  children : List UISurface

export
Show UISurface where
  show (MkUISurface id surfaceParameters state children)
    =  "{ id: " ++ show id
    ++ ", surfaceParameters: " ++ show surfaceParameters
    ++ ", state: " ++ show state
    ++ ", children: " ++ show children
    ++ " }"

export
getDimensions : UISurface -> (Int, Int)
getDimensions surface = fromMaybe (0, 0) $ dimensions (surfaceParameters surface)

setDimensions : (Int, Int) -> UISurface -> UISurface
setDimensions dims = record {
  surfaceParameters $= record { dimensions = Just dims }
}

setChildren : List UISurface -> UISurface -> UISurface
setChildren xs = record { children = xs }

public export
data Root = MkRoot Int Int UISurface -- x, y

export
Show Root where
  show (MkRoot x y s) = "(" ++ show x ++ ", " ++ show y ++ ") with " ++ show s

public export
record PUI where
  constructor MkPUI
  preload : PreloadResults
  idCounter : Nat
  shown : DDict ContentReference Root
  hidden : DDict ContentReference Root
  clicks : List String

export
initialPUI : PreloadResults -> PUI
initialPUI preload = MkPUI preload Z empty empty empty

export
nextId : PUI -> PUI
nextId = record { idCounter $= S }

export
puiAddRoot : (ref : ContentReference) ->
             (x : Int) ->
             (y : Int) ->
             (surface : UISurface) ->
             PUI -> PUI
puiAddRoot ref x y surface = let root = MkRoot x y surface
                                 in record { hidden $= insert ref root }

export
puiRemoveRoot : (ref : ContentReference) -> PUI -> PUI
puiRemoveRoot ref = record { shown $= delete ref, hidden $= delete ref }

export
dimensionsFromChildren : UISurface -> List (Int, Int) -> (Int, Int)
dimensionsFromChildren surface xs
  = let widths = map fst xs
        heights = map snd xs
        in case layout (surfaceParameters surface) of
            Vertical => let x = fromMaybe 14 $ head' $ sort widths
                            y = sum heights
                            in (x, y)
            Horizontal => let x = sum widths
                              y = fromMaybe 13 $ head' $ sort heights
                              in (x, y)

export
decideDimensions : UISurface -> List (Int, Int) -> (Int, Int)
decideDimensions surface xs = case dimensions (surfaceParameters surface) of
  Nothing => dimensionsFromChildren surface xs
  Just x => x

export
refreshSurface : UISurface -> (UISurface, (Int, Int))
refreshSurface surface
  = let refreshed_children = map refreshSurface (children surface)
        children_surfaces = map fst refreshed_children
        children_dimensions = map snd refreshed_children
        newDimensions = decideDimensions surface children_dimensions
        refreshed_surface
          = (setDimensions newDimensions . setChildren children_surfaces) surface
        in (refreshed_surface, newDimensions)

export
refresh : Root -> Root
refresh (MkRoot x y surface) = let (refreshed_surface, _) = refreshSurface surface
                                   in MkRoot x y refreshed_surface

export
puiRefreshRoot : (ref : ContentReference) -> PUI -> PUI
puiRefreshRoot ref = record { shown $= update ref refresh,
                              hidden $= update ref refresh }

export
puiHideRoot : (ref : ContentReference) -> PUI -> PUI
puiHideRoot ref pui = case lookup ref (shown pui) of
  Nothing => pui
  Just root => record {
    shown $= delete ref,
    hidden $= insert ref root
  } pui

export
puiShowRoot : (ref : ContentReference) -> PUI -> PUI
puiShowRoot ref pui = case lookup ref (hidden pui) of
  Nothing => pui
  Just root => record {
    hidden $= delete ref,
    shown $= insert ref root
  } pui

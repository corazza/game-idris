module Client.UI.PUI

import GameIO
import Descriptions.SurfaceDescription
import JSONCache
import Objects

public export
data UIEvent
  = Click Int Int
  | MoveMouse Int Int

public export
data SurfaceState = Inactive | Hover | Clicked

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
getDimensions : UISurface -> (Int, Int)
getDimensions surface = fromMaybe (0, 0) $ dimensions (surfaceParameters surface)

setDimensions : (Int, Int) -> UISurface -> UISurface
setDimensions dims = record {
  surfaceParameters $= record { dimensions = Just dims }
}

public export
data Root = MkRoot Int Int UISurface -- x, y

public export
record PUI where
  constructor MkPUI
  preload : PreloadResults
  idCounter : Nat
  roots : DDict ContentReference Root
  clicks : List String

export
initialPUI : PreloadResults -> PUI
initialPUI preload = MkPUI preload Z empty empty

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
                                 in record { roots $= insert ref root }

export
puiRemoveRoot : (ref : ContentReference) -> PUI -> PUI
puiRemoveRoot ref = record { roots $= delete ref }

refresh : Root -> Root
refresh (MkRoot x y surface) = let (refreshed_surface, _) = refreshSurface surface
                                   in MkRoot x y refreshed_surface where
  dimensionsFromChildren : List (Int, Int) -> (Int, Int)
  dimensionsFromChildren xs
    = let widths = map fst xs
          heights = map snd xs
          in case layout (surfaceParameters surface) of
              Vertical => let x = fromMaybe 0 $ head' $ sort widths
                              y = sum heights
                              in (x, y)
              Horizontal => let x = sum widths
                                y = fromMaybe 0 $ head' $ sort heights
                                in (x, y)

  decideDimensions : List (Int, Int) -> (Int, Int)
  decideDimensions xs = case dimensions (surfaceParameters surface) of
    Nothing => dimensionsFromChildren xs
    (Just x) => x

  refreshSurface : UISurface -> (UISurface, (Int, Int))
  refreshSurface surface
    = let refreshed_children = map refreshSurface (children surface)
          children_surfaces = map fst refreshed_children
          children_dimensions = map snd refreshed_children
          newDimensions = decideDimensions children_dimensions
          in (setDimensions newDimensions surface, newDimensions)

export
puiRefreshRoot : (ref : ContentReference) -> PUI -> PUI
puiRefreshRoot ref = record { roots $= update ref refresh }

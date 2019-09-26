module Client.UI.PUI

import Graphics.SDL2

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

public export
record UISurface where
  constructor MkUISurface
  id : SurfaceId
  name : SurfaceName
  surfaceParameters : SurfaceParameters
  dimensions : (Int, Int)
  state : SurfaceState
  children : List UISurface
  rect : SDLRect -- calculated in refresh, used for clicks and rendering
%name UISurface surface

export
Show UISurface where
  show (MkUISurface id name surfaceParameters dimensions state children rect)
    =  "{ id: " ++ id
    ++ ", name: " ++ name
    ++ ", surfaceParameters: " ++ show surfaceParameters
    ++ ", dimensions: " ++ show dimensions
    ++ ", state: " ++ show state
    ++ ", rect: " ++ show rect
    ++ ", children: " ++ show children
    ++ " }"

layout : UISurface -> Layout
layout = layout . surfaceParameters

setDimensions : (Int, Int) -> UISurface -> UISurface
setDimensions dims = record {
  dimensions = dims
  -- surfaceParameters $= record { dimensions = Just dims }
}

export
setRenderDescription : Maybe SurfaceRenderDescription -> UISurface -> UISurface
setRenderDescription desc = record {
  surfaceParameters $= record { render = desc }
}

-- export
-- resetDimensions : UISurface -> UISurface
-- resetDimensions = record { surfaceParameters $= record { dimensions = Nothing } }

export
setChildren : List UISurface -> UISurface -> UISurface
setChildren xs = record { children = xs }

setRect : SDLRect -> UISurface -> UISurface
setRect rect' = record { rect = rect' }

public export
data Root = MkRoot Int Int UISurface -- x, y

export
Show Root where
  show (MkRoot x y s) = "(" ++ show x ++ ", " ++ show y ++ ") with " ++ show s

export
puiSetRootSurface : UISurface -> Root -> Root
puiSetRootSurface surface (MkRoot x y s) = MkRoot x y surface

isChild : SurfaceName -> (SurfaceName, UISurface) -> Bool
isChild x (x', surface) = x == x'

transformFind : List (List (SurfaceName, UISurface)) ->
                (SurfaceName, UISurface) ->
                (List UISurface, UISurface, List UISurface)
transformFind [xs1, xs2] x = (map snd xs1, snd x, map snd xs2)
-- transformFind xs x = ?

findChild : SurfaceName ->
            List (SurfaceName, UISurface) ->
            Maybe (List UISurface, UISurface, List UISurface)
findChild x name_child = let found = head' $ filter (isChild x) name_child
                             without = split (isChild x) name_child
                             in map (transformFind without) found

updateSubsurface : List SurfaceName ->
                   (f : UISurface -> UISurface) ->
                   UISurface ->
                   UISurface
updateSubsurface [] f surface = f surface
updateSubsurface (x::xs) f surface
  = let children = children surface
        names = map UISurface.name children
        name_child = zip names children
        in case findChild x name_child of
          Nothing => surface
          Just (first, child_matching_id, rest) =>
            let updated_matching = updateSubsurface xs f child_matching_id
                new_children = concat [first, updated_matching :: rest]
                in setChildren new_children surface

export
updateSurfaceInRoot : List SurfaceName -> (f : UISurface -> UISurface) -> Root -> Root
updateSurfaceInRoot names f (MkRoot x y s) = MkRoot x y (updateSubsurface names f s)

export
surfaceToRoot : (f : UISurface -> UISurface) -> Root -> Root
surfaceToRoot f (MkRoot x y surface) = MkRoot x y (f surface)

public export
record PUI where
  constructor MkPUI
  preload : PreloadResults
  idCounter : Nat
  shown : DDict ContentReference Root
  hidden : DDict ContentReference Root
  clicks : List Click

export
initialPUI : PreloadResults -> PUI
initialPUI preload = MkPUI preload Z empty empty empty

export
flushClicks : PUI -> PUI
flushClicks = record { clicks = empty }

export
nextId : PUI -> PUI
nextId = record { idCounter $= S }

export
puiAddClick : (click : Click) -> PUI -> PUI
puiAddClick click = record { clicks $= append click }

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

dimensionsFromChildren : UISurface -> List (Int, Int) -> (Int, Int)
dimensionsFromChildren surface xs
  = let widths = map fst xs
        heights = map snd xs
        in case layout (surfaceParameters surface) of
            Vertical => let x = fromMaybe 0 $ head' $ sortBy (flip compare) widths
                            y = sum heights
                            in (x, y)
            Horizontal => let x = sum widths
                              y = fromMaybe 0 $ head' $ sortBy (flip compare) heights
                              in (x, y)

decideDimensions : UISurface -> List (Int, Int) -> (Int, Int)
decideDimensions surface xs = case dimensions (surfaceParameters surface) of
  Nothing => dimensionsFromChildren surface xs
  Just x => x

refreshSurface : UISurface -> (UISurface, (Int, Int))
refreshSurface surface
  = let refreshed_children = map refreshSurface (children surface)
        children_surfaces = map fst refreshed_children
        children_dimensions = map snd refreshed_children
        newDimensions = decideDimensions surface children_dimensions
        refreshed_surface
          = (setDimensions newDimensions . setChildren children_surfaces) surface
        in (refreshed_surface, newDimensions)

mutual
  refreshRect : (Int, Int) -> UISurface -> UISurface
  refreshRect xy@(x, y) surface
    = let refreshed_children = refreshChildrenRect xy (layout surface) (children surface) []
          (width, height) = dimensions surface
          rect = MkSDLRect x y width height
          in (setRect rect . setChildren refreshed_children) surface

  -- TODO foldr
  refreshChildrenRect : (upperleft: (Int, Int)) ->
                        Layout ->
                        (children : List UISurface) ->
                        (acc : List UISurface) ->
                        List UISurface
  refreshChildrenRect (x, y) layout [] acc = reverse acc
  refreshChildrenRect (x, y) layout (surface::xs) acc
    = let refreshed = refreshRect (x, y) surface
          (MkSDLRect x y width height) = rect refreshed
          in case layout of
            Vertical => refreshChildrenRect (x, y+height) layout xs (refreshed::acc)
            Horizontal => refreshChildrenRect (x+width, y) layout xs (refreshed::acc)

refresh : Root -> Root
refresh (MkRoot x y surface) = let (refreshed_surface, _) = refreshSurface surface
                                   with_rect = refreshRect (x, y) refreshed_surface
                                   in MkRoot x y with_rect

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

export
puiUpdateRoot : (ref : ContentReference) -> (f : Root -> Root) -> PUI -> PUI
puiUpdateRoot ref f = record {
  hidden $= update ref f,
  shown $= update ref f
}

inRect : (xy : (Int, Int)) -> SDLRect -> Bool
inRect (x, y) (MkSDLRect rx ry rw rh)
  =  x >= rx
  && x <= rx + rw
  && y >= ry
  && y <= ry + rh

getClickFromSurface : (xy : (Int, Int)) ->
                      UISurface ->
                      Maybe Click
getClickFromSurface xy surface = case inRect xy (rect surface) of
  False => Nothing
  True => let children_results = map (getClickFromSurface xy) (children surface)
              final_result = head' $ catMaybes children_results
              in fromMaybe (click (surfaceParameters surface)) (map Just final_result)

getClickFromRoot : (xy : (Int, Int)) -> Root -> Maybe Click
getClickFromRoot xy (MkRoot rx ry surface) = getClickFromSurface xy surface

export
getClick : (xy : (Int, Int)) -> List Root -> Maybe Click
getClick xy = head' . catMaybes . map (getClickFromRoot xy)

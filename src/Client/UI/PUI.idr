module Client.UI.PUI

import GameIO
import Descriptions.SurfaceDescription
import JSONCache
import Objects
import Client.SDL

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
  surfaceParameters : SurfaceParameters
  state : SurfaceState
  children : List UISurface
  rect : SDLRect -- calculated in refresh, used for clicks and rendering

export
Show UISurface where
  show (MkUISurface id surfaceParameters state children rect)
    =  "{ id: " ++ show id
    ++ ", surfaceParameters: " ++ show surfaceParameters
    ++ ", state: " ++ show state
    ++ ", children: " ++ show children
    ++ ", rect:" ++ show rect
    ++ " }"

layout : UISurface -> Layout
layout = layout . surfaceParameters

export
getDimensions : UISurface -> (Int, Int)
getDimensions surface = fromMaybe (0, 0) $ dimensions (surfaceParameters surface)

setDimensions : (Int, Int) -> UISurface -> UISurface
setDimensions dims = record {
  surfaceParameters $= record { dimensions = Just dims }
}

setChildren : List UISurface -> UISurface -> UISurface
setChildren xs = record { children = xs }

setRect : SDLRect -> UISurface -> UISurface
setRect rect' = record { rect = rect' }

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
flushClicks : PUI -> PUI
flushClicks = record { clicks = empty }

export
nextId : PUI -> PUI
nextId = record { idCounter $= S }

export
puiAddClick : (click : String) -> PUI -> PUI
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
            Vertical => let x = fromMaybe 14 $ head' $ sort widths
                            y = sum heights
                            in (x, y)
            Horizontal => let x = sum widths
                              y = fromMaybe 13 $ head' $ sort heights
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
          (width, height) = getDimensions surface
          rect = MkSDLRect x y width height
          in (setRect rect . setChildren refreshed_children) surface

  -- TODO foldr
  refreshChildrenRect : (upperleft: (Int, Int)) ->
                        Layout ->
                        (children : List UISurface) ->
                        (acc : List UISurface) ->
                        List UISurface
  refreshChildrenRect (x, y) layout [] acc = acc
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

inRect : (xy : (Int, Int)) -> SDLRect -> Bool
inRect (x, y) (MkSDLRect rx ry rw rh)
  =  x >= rx
  && x <= rx + rw
  && y >= ry
  && y <= ry + rh

getClickFromSurface : (xy : (Int, Int)) ->
                      UISurface ->
                      Maybe String
getClickFromSurface xy surface = case inRect xy (rect surface) of
  False => Nothing
  True => let children_results = map (getClickFromSurface xy) (children surface)
              final_result = head' $ catMaybes children_results
              in fromMaybe (click (surfaceParameters surface)) (map Just final_result)

getClickFromRoot : (xy : (Int, Int)) -> Root -> Maybe String
getClickFromRoot xy (MkRoot rx ry surface) = getClickFromSurface xy surface

export
getClick : (xy : (Int, Int)) -> List Root -> Maybe String
getClick xy = head' . catMaybes . map (getClickFromRoot xy)

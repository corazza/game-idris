module Client.UI

import Control.ST
import Control.ST.ImplicitCall

import GameIO
import Client.UI.PUI
import Client.SDL
import Descriptions.SurfaceDescription
import Descriptions.Color
import JSONCache
import Objects
import Exception

public export
interface UI (m : Type -> Type) where
  SUI : Type

  startUI : (preload : PreloadResults) -> ST m Var [add SUI]
  endUI : (ui : Var) -> ST m () [remove ui SUI]

  queryPUI : (ui : Var) ->
             (q : PUI -> a) ->
             ST m a [ui ::: SUI]

  updatePUI : (ui : Var) ->
              (f : PUI -> PUI) ->
              ST m () [ui ::: SUI]


  newId : (ui : Var) -> ST m SurfaceId [ui ::: SUI]
  decideId : (ui : Var) ->
             (desc : SurfaceDescription) ->
             ST m SurfaceId [ui ::: SUI]

  surfaceFromDesc : (ui : Var) ->
                    (desc : SurfaceDescription) ->
                    ST m UISurface [ui ::: SUI]
  surfacesFromDescs : (ui : Var) ->
                      (descs : List SurfaceDescription) ->
                      ST m (List UISurface) [ui ::: SUI]

  addRoot : (ui : Var) ->
            (ref : ContentReference) ->
            (x : Int) ->
            (y : Int) ->
            ST m () [ui ::: SUI]
  removeRoot : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]
  toggleRoot : (ui : Var) ->
               (ref : ContentReference) ->
               (x : Int) ->
               (y : Int) ->
               ST m () [ui ::: SUI]

  refreshRoot : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]

  processEvent : (ui : Var) -> (event : UIEvent) -> ST m () [ui ::: SUI]

  getClicks : (ui : Var) -> ST m (List String) [ui ::: SUI]

export
(GameIO m, SDL m) => UI m where
  SUI = State PUI

  startUI preload = new $ initialPUI preload
  endUI ui = delete ui

  queryPUI ui q = with ST do
    ui' <- read ui
    pure $ q ui'

  updatePUI ui f = update ui f

  newId ui = with ST do
    idNum <- queryPUI ui idCounter
    updatePUI ui nextId
    pure $ "ui_autoid_" ++ show idNum

  decideId ui desc = case SurfaceDescription.id desc of
    Nothing => newId ui
    Just x => pure x

  surfaceFromDesc ui desc = with ST do
    children <- surfacesFromDescs ui (children desc)
    id <- decideId ui desc
    pure $ MkUISurface id (surfaceParameters desc) Inactive children

  surfacesFromDescs ui [] = pure []
  surfacesFromDescs ui (desc::xs)
    = pure $ !(surfaceFromDesc ui desc) :: !(surfacesFromDescs ui xs)

  addRoot ui ref x y = with ST do
    preload' <- queryPUI ui preload
    case getSurfaceDescription ref preload' of
      Left e => lift $ log $
        "couldn't load surface description " ++ ref ++ ", error: " ++ e
      Right desc => with ST do
        surface <- surfaceFromDesc ui desc
        updatePUI ui $ puiAddRoot ref x y surface

  removeRoot ui ref = updatePUI ui $ puiRemoveRoot ref

  toggleRoot ui ref x y = with ST do
    refreshRoot ui ref
    roots' <- queryPUI ui $ roots
    case hasKey ref roots' of
      False => addRoot ui ref x y
      True => removeRoot ui ref

  refreshRoot ui ref = updatePUI ui $ puiRefreshRoot ref

renderRoot : (SDL m, UI m, GameIO m) =>
             (ui : Var) ->
             (sdl : Var) ->
             (root : Root) ->
             ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
renderRoot ui sdl (MkRoot x y surface) = with ST do
  let (width, height) = getDimensions surface
  lift $ log $ show (width, height)
  let rect = MkSDLRect x y width height
  filledRect sdl rect (MkColor 144 144 144 33)

renderRoots : (SDL m, UI m, GameIO m) =>
              (ui : Var) ->
              (sdl : Var) ->
              (roots : List Root) ->
              ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
renderRoots ui sdl [] = pure ()
renderRoots ui sdl (root::xs) = renderRoot ui sdl root >>= const (renderRoots ui sdl xs)

export
renderUI : (SDL m, UI m, GameIO m) =>
           (ui : Var) ->
           (sdl : Var) ->
           ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
renderUI ui sdl = queryPUI ui roots >>= renderRoots ui sdl . toList

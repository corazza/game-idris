module Client.UI

import Control.ST
import Control.ST.ImplicitCall

import GameIO
import Client.UI.PUI
import Client.SDL
import Client.ClientCommands
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

  toggleExistent : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]
  hideRoot : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]
  showRoot : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]
  refreshRoot : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]
  updateRoot : (ui : Var) ->
               (ref : ContentReference) ->
               (f : Root -> Root) ->
               ST m () [ui ::: SUI]
  setRootSurface : (ui : Var) ->
                   (ref : ContentReference) ->
                   (surface_desc : SurfaceDescription) ->
                   ST m () [ui ::: SUI]
  setRootChildren : (ui : Var) ->
                    (ref : ContentReference) ->
                    (surface_descs : List SurfaceDescription) ->
                    ST m () [ui ::: SUI]
  logRoot : (ui : Var) -> (ref : ContentReference) -> ST m () [ui ::: SUI]


  processCommand : (ui : Var) ->
                   (command : ClientCommand) ->
                   ST m (Either () ClientCommand) [ui ::: SUI]

  eatClientCommands : (ui : Var) ->
                      (commands : List ClientCommand) ->
                      ST m (List ClientCommand) [ui ::: SUI]

  eatClientCommands' : (ui : Var) ->
                       (commands : List ClientCommand) ->
                       (acc : List ClientCommand) ->
                       ST m (List ClientCommand) [ui ::: SUI]

  processMouseEvent : (ui : Var) ->
                      (event : MouseEvent) ->
                      ST m (Either () MouseEvent) [ui ::: SUI]

  addClick : (ui : Var) -> (click : Click) -> ST m () [ui ::: SUI]
  getClicks : (ui : Var) -> ST m (List Click) [ui ::: SUI]

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
    pure $ MkUISurface id (surfaceParameters desc) Inactive children (MkSDLRect 0 0 0 0)

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
        refreshRoot ui ref

  removeRoot ui ref = updatePUI ui $ puiRemoveRoot ref

  toggleRoot ui ref x y = with ST do
    refreshRoot ui ref
    shown' <- queryPUI ui shown
    hidden' <- queryPUI ui hidden
    case (hasKey ref shown', hasKey ref hidden') of
      (False, False) => with ST do
        addRoot ui ref x y
        showRoot ui ref
      _ => toggleExistent ui ref

  toggleExistent ui ref = with ST do
    shown' <- queryPUI ui shown
    hidden' <- queryPUI ui hidden
    case (lookup ref shown', lookup ref hidden') of
      (Just root, Nothing) => hideRoot ui ref
      (Nothing, Just root) => showRoot ui ref
      _ => pure ()

  refreshRoot ui ref = updatePUI ui $ puiRefreshRoot ref
  hideRoot ui ref = updatePUI ui $ puiHideRoot ref
  showRoot ui ref = updatePUI ui $ puiShowRoot ref

  updateRoot ui ref f = with ST do
    updatePUI ui $ puiUpdateRoot ref f
    refreshRoot ui ref

  setRootSurface ui ref surface_desc = with ST do
    surface <- surfaceFromDesc ui surface_desc
    updateRoot ui ref $ puiSetRootSurface surface
    updateRoot ui ref $ surfaceToRoot resetDimensions

  setRootChildren ui ref surface_descs = with ST do
    surfaces <- surfacesFromDescs ui surface_descs
    updateRoot ui ref $ surfaceToRoot $ setChildren surfaces
    updateRoot ui ref $ surfaceToRoot resetDimensions

  logRoot ui ref = with ST do
    shown' <- queryPUI ui shown
    hidden' <- queryPUI ui hidden
    case (lookup ref shown', lookup ref hidden') of
      (Just root, Nothing) => lift $ log $ show root
      (Nothing, Just root) => lift $ log $ show root
      _ => pure ()

  processCommand ui (Stop MainMenu) = with ST do
    toggleRoot ui "main/ui/main_menu.json" 100 100
    pure $ Left ()
  processCommand ui command@(Stop Inventory) = with ST do
    toggleRoot ui "main/ui/inventory.json" 500 200
    pure $ Right command -- not eaten because the client has to feed further info
  processCommand ui (Mouse x) = pure $ map Mouse !(processMouseEvent ui x)
  processCommand ui command = pure $ Right command

  eatClientCommands ui xs = eatClientCommands' ui xs []

  eatClientCommands' ui [] acc = pure acc
  eatClientCommands' ui (cmd::xs) acc = case !(processCommand ui cmd) of
    Left () => eatClientCommands' ui xs acc
    Right r => eatClientCommands' ui xs (append r acc)

  processMouseEvent ui event@(ButtonUp x y) = with ST do
    shown' <- queryPUI ui shown
    let roots = map snd $ toList shown'
    case getClick (x, y) roots of
      Nothing => pure $ Right event
      Just click_string => with ST do
        addClick ui click_string
        pure $ Left () -- sdl event was consumed
  processMouseEvent ui ev@(ButtonDown x y) = pure $ Right ev
  processMouseEvent ui ev@(Move x y) = pure $ Right ev

  addClick ui click = updatePUI ui $ puiAddClick click

  getClicks ui = with ST do
    clicks' <- queryPUI ui clicks
    updatePUI ui flushClicks
    pure clicks'

executeMethod : (SDL m, UI m, GameIO m) =>
                (ui : Var) ->
                (sdl : Var) ->
                (rect : SDLRect) ->
                (method : SurfaceRenderMethod) ->
                ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
executeMethod ui sdl rect (Image x) = drawWholeCenter sdl x rect 0.0 0
executeMethod ui sdl rect (Colored color) = filledRect sdl rect color
executeMethod ui sdl rect (Text string size color)
  = drawText sdl string size color rect

pickRenderMethod : SurfaceState -> SurfaceRenderDescription -> SurfaceRenderMethod
pickRenderMethod Inactive desc = inactive desc
pickRenderMethod Hover desc = fromMaybe (inactive desc) (hover desc)
pickRenderMethod Clicked desc = fromMaybe (inactive desc) (clicked desc)

mutual
  renderSurface : (SDL m, UI m, GameIO m) =>
                  (ui : Var) ->
                  (sdl : Var) ->
                  (surface : UISurface) ->
                  ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
  renderSurface ui sdl surface
    =  let render_desc = render $ surfaceParameters surface
           layout = layout $ surfaceParameters surface
           render_method = pickRenderMethod (state surface) render_desc
           in with ST do
             executeMethod ui sdl (rect surface) render_method
             renderSurfaces ui sdl (children surface) layout

  renderSurfaces : (SDL m, UI m, GameIO m) =>
                   (ui : Var) ->
                   (sdl : Var) ->
                   (surfaces : List UISurface) ->
                   (layout : Layout) ->
                   ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
  renderSurfaces ui sdl [] layout = pure ()
  renderSurfaces ui sdl (surface::xs) layout = with ST do
    renderSurface ui sdl surface
    renderSurfaces ui sdl xs layout

renderRoot : (SDL m, UI m, GameIO m) =>
             (ui : Var) ->
             (sdl : Var) ->
             (root : Root) ->
             ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
renderRoot ui sdl root@(MkRoot x y surface)
  = renderSurface ui sdl surface >>= const (pure ())
renderRoot ui sdl _ = pure ()

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
renderUI ui sdl = queryPUI ui shown >>= renderRoots ui sdl . toList
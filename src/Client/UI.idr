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

  processCommand ui (Stop MainMenu) = with ST do
    toggleRoot ui "main/ui/main_menu.json" 100 100
    pure $ Left ()
  processCommand ui (Mouse x) = pure $ map Mouse !(processMouseEvent ui x)
  processCommand ui command = pure $ Right command

  eatClientCommands ui xs = eatClientCommands' ui xs []

  eatClientCommands' ui [] acc = pure acc
  eatClientCommands' ui (cmd::xs) acc = case !(processCommand ui cmd) of
    Left () => eatClientCommands' ui xs acc
    Right r => eatClientCommands' ui xs (append r acc)

  processMouseEvent ui (ButtonUp x y) = ?sdfkksdgfjgn_1
  processMouseEvent ui (ButtonDown x y) = ?sdfkksdgfjgn_2
  processMouseEvent ui (Move x y) = ?sdfkksdgfjgn_3

executeMethod : (SDL m, UI m, GameIO m) =>
                (ui : Var) ->
                (sdl : Var) ->
                (rect : SDLRect) ->
                (method : SurfaceRenderMethod) ->
                ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
executeMethod ui sdl rect (Image x) = drawWholeCenter sdl x rect 0.0 0
executeMethod ui sdl rect (Colored color) = filledRect sdl rect color

pickRenderMethod : SurfaceState -> SurfaceRenderDescription -> SurfaceRenderMethod
pickRenderMethod Inactive desc = inactive desc
pickRenderMethod Hover desc = fromMaybe (inactive desc) (hover desc)
pickRenderMethod Clicked desc = fromMaybe (inactive desc) (clicked desc)

mutual
  renderSurface : (SDL m, UI m, GameIO m) =>
                  (ui : Var) ->
                  (sdl : Var) ->
                  (surface : UISurface) ->
                  (xy : (Int, Int)) ->
                  ST m (Int, Int) [ui ::: SUI {m}, sdl ::: SSDL {m}]
  renderSurface ui sdl surface (x, y)
    =  let (width, height) = getDimensions surface
           rect = MkSDLRect x y width height
           render_desc = render $ surfaceParameters surface
           layout = layout $ surfaceParameters surface
           render_method = pickRenderMethod (state surface) render_desc
           in with ST do
             executeMethod ui sdl rect render_method
             renderSurfaces ui sdl (children surface) layout (x, y)
             pure (width, height)

  renderSurfaces : (SDL m, UI m, GameIO m) =>
                   (ui : Var) ->
                   (sdl : Var) ->
                   (surfaces : List UISurface) ->
                   (layout : Layout) ->
                   (xy : (Int, Int)) ->
                   ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
  renderSurfaces ui sdl [] layout (x, y) = pure ()
  renderSurfaces ui sdl (surface::xs) layout (x, y) = with ST do
    (width, height) <- renderSurface ui sdl surface (x, y)
    case layout of
      Vertical => renderSurfaces ui sdl xs layout (x, y+height)
      Horizontal => renderSurfaces ui sdl xs layout (x+width, y)

renderRoot : (SDL m, UI m, GameIO m) =>
             (ui : Var) ->
             (sdl : Var) ->
             (root : Root) ->
             ST m () [ui ::: SUI {m}, sdl ::: SSDL {m}]
renderRoot ui sdl root@(MkRoot x y surface)
  = renderSurface ui sdl surface (x, y) >>= const (pure ())
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

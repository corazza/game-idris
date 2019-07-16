module Client.Rendering

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Client.Rendering.Camera
import Client.Rendering.PRendering
import Client.SDL
import Dynamics
import Dynamics.PDynamics
import GameIO
import Exception
import Settings
import Objects
import Descriptions
import Descriptions.ObjectDescription.RenderDescription
import Commands

public export
interface SDL m => Rendering (m : Type -> Type) where
  SRendering : Type

  startRendering : (settings : RenderingSettings) -> Background -> ST m Var [add SRendering]
  endRendering : (rendering : Var) -> ST m () [remove rendering SRendering]

  queryPRendering : (rendering : Var) -> (q : PRendering -> a) -> ST m a [rendering ::: SRendering]
  getCamera : (rendering : Var) -> ST m Camera [rendering ::: SRendering]

  addObject : (rendering : Var) ->
              (id : ObjectId) ->
              (desc : RenderDescription) ->
              (layer : Nat) ->
              ST m () [rendering ::: SRendering]

  removeObject : (rendering : Var) -> (id : ObjectId) -> ST m () [rendering ::: SRendering]

  processCommand : (rendering : Var) -> (command : Command) -> ST m () [rendering ::: SRendering]

export
(GameIO m, SDL m) => Rendering m where
  SRendering = State PRendering

  startRendering settings background
    = let camera = fromSettings $ cameraSettings settings
          in new $ MkPRendering background empty empty camera

  endRendering rendering = delete rendering

  queryPRendering rendering q = read rendering >>= pure . q
  getCamera rendering = queryPRendering rendering camera

  addObject rendering id desc layer = with ST do
    update rendering $ addToLayer id desc layer
    case desc of
      Animated stateDict => ticks >>= update rendering . addInitialAnimationState id
      _ => pure ()

  removeObject rendering id = with ST do
    update rendering $ removeFromLayers id
    update rendering $ removeAnimationState id

  processCommand rendering (Start (Movement Left) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  processCommand rendering (Start (Movement Right) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  processCommand rendering (Start (Movement Up) id) = pure ()
  processCommand rendering (Start (Movement Down) id) = pure ()
  processCommand rendering (Start (Attack x) id) = pure ()
  processCommand rendering (Start Walk id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  processCommand rendering (Stop (Movement Left) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  processCommand rendering (Stop (Movement Right) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  processCommand rendering (Stop (Movement Up) id) = pure ()
  processCommand rendering (Stop (Movement Down) id) = pure ()
  processCommand rendering (Stop (Attack x) id) = pure ()
  processCommand rendering (Stop Walk id) = pure ()

-- HERE set bodyData in PRendering, lookup from there, to save on arguments here

renderBackground : SDL m =>
                   (sdl : Var) ->
                   (camera : Camera) ->
                   (background : Background) ->
                   ST m () [sdl ::: SSDL {m}]
renderBackground sdl camera (MkBackground image dim) = with ST do
  let (w, h) = dimToScreen camera (2 `scale` dim)
  let (w', h') = dimToScreen camera dim
  let (x, y) = positionToScreen camera (0, 0)
  drawWholeCenter sdl image (MkSDLRect (x - w') (y - h') w h) 0.0 0

renderObject : SDL m => Rendering m =>
               (rendering : Var) ->
               (sdl : Var) ->
               (camera : Camera) ->
               (body_data : BodyData) ->
               (rendering_description : RenderDescription) ->
               ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderObject rendering sdl camera body_data Invisible = pure ()
renderObject rendering sdl camera body_data (Tiled ref tileDims nxny) = ?sdjkfsdn_2
renderObject rendering sdl camera body_data (Colored color) = pure () -- TODO color needs dim parameter
renderObject rendering sdl camera body_data (Single ref dims) = ?sdjkfsdn_4
renderObject rendering sdl camera body_data (Animated x) = ?sdjkfsdn_5


renderLayer : SDL m => Rendering m =>
              (rendering : Var) ->
              (sdl : Var) ->
              (bodyData : Objects BodyData) ->
              (layer : List (ObjectId, RenderDescription)) ->
              ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderLayer rendering sdl bodyData [] = pure ()
renderLayer rendering sdl bodyData ((id, render_description)::xs)
  = case lookup id bodyData of
      Nothing => renderLayer rendering sdl bodyData xs
      Just body_data => with ST do
        camera <- queryPRendering rendering camera
        renderObject rendering sdl camera body_data render_description
        renderLayer rendering sdl bodyData xs

renderLayers : SDL m => Rendering m =>
               (rendering : Var) ->
               (sdl : Var) ->
               (bodyData : Objects BodyData) ->
               (layers : List (List (ObjectId, RenderDescription))) ->
               ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderLayers rendering sdl bodyData [] = pure ()
renderLayers rendering sdl bodyData (layer::xs)
  = renderLayer rendering sdl bodyData layer >>=
      const (renderLayers rendering sdl bodyData xs)

export
render : (SDL m, Rendering m) =>
         (rendering : Var) ->
         (sdl : Var) ->
         (bodyData : Objects BodyData) ->
         ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
render rendering sdl bodyData = with ST do
  camera <- queryPRendering rendering camera
  background <- queryPRendering rendering background
  renderBackground sdl camera background
  renderLayers rendering sdl bodyData !(queryPRendering rendering layerList)

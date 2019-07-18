module Client.Rendering

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Client.Rendering.Camera
import Client.Rendering.PRendering
import Client.SDL
import Dynamics
import JSONCache
import Dynamics.PDynamics
import Dynamics.Control
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

  startRendering : (settings : RenderingSettings) -> Background -> PreloadResults -> ST m Var [add SRendering]
  endRendering : (rendering : Var) -> ST m () [remove rendering SRendering]

  queryPRendering : (rendering : Var) -> (q : PRendering -> a) -> ST m a [rendering ::: SRendering]
  getCamera : (rendering : Var) -> ST m Camera [rendering ::: SRendering]

  addObject : (rendering : Var) ->
              (id : ObjectId) ->
              (desc : RenderDescription) ->
              (layer : Nat) ->
              ST m () [rendering ::: SRendering]

  removeObject : (rendering : Var) -> (id : ObjectId) -> ST m () [rendering ::: SRendering]

  runCommand : (rendering : Var) -> (command : Command) -> ST m () [rendering ::: SRendering]

  updateBodyData : (rendering : Var) -> (bodyData : Objects BodyData) -> ST m () [rendering ::: SRendering]

  follow : (rendering : Var) -> ObjectId -> ST m () [rendering ::: SRendering]

export
(GameIO m, SDL m) => Rendering m where
  SRendering = State PRendering

  startRendering settings background preload
    = let camera = fromSettings $ cameraSettings settings
          in new $ prenderingInitial background camera preload

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

  runCommand rendering (Start (Movement Left) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  runCommand rendering (Start (Movement Right) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  runCommand rendering (Start (Movement Up) id) = pure ()
  runCommand rendering (Start (Movement Down) id) = pure ()
  runCommand rendering (Start (Attack x) id) = pure ()
  runCommand rendering (Start Walk id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  runCommand rendering (Stop (Movement Left) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  runCommand rendering (Stop (Movement Right) id) = ticks >>=
    update rendering . setAnimationState id . MkAnimationState
  runCommand rendering (Stop (Movement Up) id) = pure ()
  runCommand rendering (Stop (Movement Down) id) = pure ()
  runCommand rendering (Stop (Attack x) id) = pure ()
  runCommand rendering (Stop Walk id) = pure ()

  updateBodyData rendering bodyData =
    update rendering $ (updateCamera . setBodyData bodyData)

  follow rendering id = update rendering $ setFollowing id


missingStateError : (state : String) -> (for : ObjectId) -> String
missingStateError state for
  = "couldn't get animation parameters for state \"" ++ state ++ "\""  ++ " for " ++ for

missingDescription : (ref : ContentReference) ->
                     (for : ObjectId) ->
                     (error : String) ->
                     String
missingDescription ref for error
  =  "missing animation description " ++ ref ++ " for " ++ for ++ " error:\n" ++ error

-- 0 no flip, 1 vertical, 2 horizontal, 3 both
getFlip : BodyData -> AnimationDescription -> Int
getFlip body_data animation_description
  = case (facingRight animation_description, forceDirection body_data) of
      (True, Leftward) => 2
      (True, Rightward) => 0
      (False, Leftward) => 0
      (False, Rightward) => 2

getFlip' : BodyData -> Int
getFlip' body_data = case forceDirection body_data of
  Leftward => 2
  Rightward => 0

getDegAngle : BodyData -> Double
getDegAngle body_data = -(angle body_data) / (2.0*pi) * 360.0

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

tile : SDL m =>
       (sdl : Var) ->
       (camera : Camera) ->
       (texture : ContentReference) ->
       (position : (Int, Int)) ->
       (tileDims : (Int, Int)) ->
       (howMany : (Nat, Nat)) ->
       ST m () [sdl ::: SSDL {m}]
tile sdl camera texture (x, y) (w, h) (nx, Z) = pure ()
tile {m} sdl camera texture (x, y) (w, h) (nx, S ny) = (with ST do
  tileRow (x, y) nx
  tile sdl camera texture (x, y + h) (w, h) (nx, ny)) where
    tileRow : (position : (Int, Int)) -> (k : Nat) -> ST m () [sdl ::: SSDL {m}]
    tileRow _ Z = pure ()
    tileRow (x', y') (S k) = with ST do
      let rect = MkSDLRect x' y' w h
      drawWholeCenter sdl texture rect 0.0 0
      tileRow (x' + w, y') k

renderObject : SDL m => Rendering m => GameIO m =>
               (rendering : Var) ->
               (sdl : Var) ->
               (camera : Camera) ->
               (id : ObjectId) ->
               (body_data : BodyData) ->
               (rendering_description : RenderDescription) ->
               ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderObject rendering sdl camera id body_data Invisible = pure ()
renderObject rendering sdl camera id body_data (Tiled ref tileDims@(w, h) howMany@(nx, ny))
  = let tileDimsFull = dimToScreen camera (2 `scale` tileDims)
        topleft = position body_data - (cast nx * w, - cast ny * h)
        initial = positionToScreen camera topleft
        in tile sdl camera ref initial tileDimsFull (nx, ny)
renderObject rendering sdl camera id body_data (Colored color) = pure () -- TODO color needs dim parameter
renderObject rendering sdl camera id body_data (Single ref dims)
  = let rect = getRect camera (position body_data) dims in
      drawWholeCenter sdl ref rect (getDegAngle body_data) (getFlip' body_data)
renderObject rendering sdl camera id body_data (Animated state_dict) = with ST do
  preload <- queryPRendering rendering preload
  let animationState = animationState body_data
  case lookup animationState state_dict of
    Nothing => lift $ log $ missingStateError animationState id
    Just aparams => let animation_ref = ref aparams in
      case getAnimationDescription animation_ref preload of
        Left e => lift $ log $ missingDescription animation_ref id e
        Right animation_description => with ST do
          Just (MkAnimationState started) <- queryPRendering rendering (getAnimationState id)
                | Nothing => lift (log $ "couldn't get animation state for " ++ id)
          let src = getSrc !ticks (fps aparams) animation_description
          let dst = getRect camera (position body_data) (dimensions aparams)
          let sheet = sheet animation_description
          let deg_angle = getDegAngle body_data
          let flip = getFlip body_data animation_description
          drawCenter sdl sheet src dst deg_angle flip

renderLayer : SDL m => Rendering m => GameIO m =>
              (rendering : Var) ->
              (sdl : Var) ->
              (layer : List (ObjectId, RenderDescription)) ->
              ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderLayer rendering sdl [] = pure ()
renderLayer rendering sdl ((id, render_description)::xs) = with ST do
  bodyData <- queryPRendering rendering bodyData
  case lookup id bodyData of
    Nothing =>  with ST do
      lift $ log id
      renderLayer rendering sdl xs
    Just body_data => with ST do
      camera <- queryPRendering rendering camera
      renderObject rendering sdl camera id body_data render_description
      renderLayer rendering sdl xs

renderLayers : SDL m => Rendering m => GameIO m =>
               (rendering : Var) ->
               (sdl : Var) ->
               (layers : List (List (ObjectId, RenderDescription))) ->
               ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderLayers rendering sdl [] = pure ()
renderLayers rendering sdl (layer::xs)
  = renderLayer rendering sdl layer >>=
      const (renderLayers rendering sdl xs)

export
render : (SDL m, Rendering m, GameIO m) =>
         (rendering : Var) ->
         (sdl : Var) ->
         ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
render rendering sdl = with ST do
  clear sdl
  camera <- queryPRendering rendering camera
  background <- queryPRendering rendering background
  renderBackground sdl camera background
  renderLayers rendering sdl !(queryPRendering rendering layerList)
  present sdl

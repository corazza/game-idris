module Client.Rendering

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D
import Graphics.SDL2

import Client.Rendering.Camera
import Client.Rendering.PRendering
import Client.Rendering.Info
import Client.SDL
import JSONCache
import GameIO
import Exception
import Settings
import Objects
import Commands
import Dynamics
import Dynamics.PDynamics
import Dynamics.DynamicsControl
import Descriptions
import Descriptions.ObjectDescription
import Descriptions.AnimationDescription
import Descriptions.MapDescription
import Descriptions.Color
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.NumericPropertyRender

public export
interface SDL m => Rendering (m : Type -> Type) where
  SRendering : Type

  startRendering : (settings : RenderingSettings) ->
                   Background ->
                   PreloadResults ->
                   ST m Var [add SRendering]
  endRendering : (rendering : Var) -> ST m () [remove rendering SRendering]

  queryPRendering : (rendering : Var) ->
                    (q : PRendering -> a) ->
                    ST m a [rendering ::: SRendering]
  updatePRendering : (rendering : Var) ->
                     (f : PRendering -> PRendering) ->
                     ST m () [rendering ::: SRendering]

  getCamera : (rendering : Var) -> ST m Camera [rendering ::: SRendering]

  addObject : (rendering : Var) ->
              (id : ObjectId) ->
              (desc : ObjectDescription) ->
              ST m () [rendering ::: SRendering]

  removeObject : (rendering : Var) -> (id : ObjectId) -> ST m () [rendering ::: SRendering]

  runCommand : (rendering : Var) -> (command : Command) -> ST m () [rendering ::: SRendering]

  updateBodyData : (rendering : Var) -> (bodyData : Objects BodyData) -> ST m () [rendering ::: SRendering]

  follow : (rendering : Var) -> ObjectId -> ST m () [rendering ::: SRendering]

  zoom : (rendering : Var) -> (x : Int) -> ST m () [rendering ::: SRendering]

  getSettings : (rendering : Var) -> ST m RenderingSettings [rendering ::: SRendering]

  private
  loadWalls : (rendering : Var) ->
              MapDescription ->
              ST m (Checked (List (WallCreation, ObjectDescription))) [rendering ::: SRendering]
  private
  addWalls : (rendering : Var) -> List (WallCreation, ObjectDescription) -> ST m () [rendering ::: SRendering]
  loadMap : (rendering : Var) -> MapDescription -> ST m () [rendering ::: SRendering]

  private
  initAnimation : (rendering : Var) ->
                  (id : ObjectId) ->
                  (desc : RenderMethod) ->
                  ST m () [rendering ::: SRendering]


export
(GameIO m, SDL m) => Rendering m where
  SRendering = State PRendering

  startRendering settings background preload
    = new $ prenderingInitial settings background preload

  endRendering rendering = delete rendering

  queryPRendering rendering q = read rendering >>= pure . q
  updatePRendering rendering f = update rendering f

  getCamera rendering = queryPRendering rendering camera

  addObject rendering id desc = case render desc of
    Nothing => pure ()
    Just render_desc => with ST do
      update rendering $ addToLayer id render_desc
      initAnimation rendering id $ method $ render_desc
      case rules desc of
        Nothing => pure ()
        Just rules_desc => update rendering $ addInfo id rules_desc

  initAnimation rendering id (Animated stateDict)
    = ticks >>= update rendering . addInitialAnimationState id
  initAnimation rendering id _ = pure ()

  removeObject rendering id = with ST do
    update rendering $ removeFromLayers id
    update rendering $ removeAnimationState id

  loadWalls rendering map_description
    = queryPRendering rendering preload >>= pure . flip getWallsAsObjects map_description

  addWalls rendering [] = pure ()
  addWalls rendering ((creation, desc)::xs)
    = addObject rendering (id creation) desc >>= const (addWalls rendering xs)

  loadMap rendering map_description = with ST do
    Right walls <- loadWalls rendering map_description | Left e => with ST do
      lift $ log $ "client couldn't get walls, error:"
      lift $ log e
    addWalls rendering walls

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
  runCommand rendering _ = pure ()

  updateBodyData rendering bodyData =
    update rendering $ (updateFollow . setBodyData bodyData)

  follow rendering id = update rendering $ setFollowing id

  zoom rendering x = let factor = if x > 0 then 1.05 else 0.95 in
    update rendering $ updateCamera $ zoomFactor $ factor -- * abs (cast x)

  getSettings rendering = with ST do
    update rendering refreshSettings
    queryPRendering rendering settings

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

renderNumericProperty : SDL m => Rendering m => GameIO m =>
                        (rendering : Var) ->
                        (sdl : Var) ->
                        (camera : Camera) ->
                        (body_data : BodyData) ->
                        (info_render : InfoRenderParameters) ->
                        (info_id : NumericPropertyId) ->
                        (info : NumericPropertyInfo) ->
                        ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderNumericProperty rendering sdl camera body_data info_render info_id info
  = with ST do
      preload <- queryPRendering rendering preload
      case getNumPropRender info_id preload of
        Left e => lift $ log $ "renderNumericProperty fail, error: " ++ e
        Right render => with ST do
          let fullWidth = lengthToScreen camera (fullWidth render)
          let (object_x, object_y) = positionToScreen camera (position body_data)
          let x = object_x - (fullWidth `div` 2)
          let yd_prop = lengthToScreen camera (yd render)
          let yd_obj = lengthToScreen camera (yd info_render)
          let y = object_y + yd_prop + yd_obj
          let w = the Int $ cast $ cast fullWidth * (current info / full info)
          let h = lengthToScreen camera (height render)
          let rect = MkSDLRect x y w h
          filledRect sdl rect (color render)

renderNumericProperties : SDL m => Rendering m => GameIO m =>
                          (rendering : Var) ->
                          (sdl : Var) ->
                          (camera : Camera) ->
                          (body_data : BodyData) ->
                          (info_render : InfoRenderParameters) ->
                          (properties : List (NumericPropertyId, NumericPropertyInfo)) ->
                          ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderNumericProperties rendering sdl camera body_data info_render [] = pure ()
renderNumericProperties rendering sdl camera body_data info_render ((id, info)::xs)
  = renderNumericProperty rendering sdl camera body_data info_render id info

renderObjectInfoAll : SDL m => Rendering m => GameIO m =>
                      (rendering : Var) ->
                      (sdl : Var) ->
                      (camera : Camera) ->
                      (body_data : BodyData) ->
                      (info_render : InfoRenderParameters) ->
                      (object_info : ObjectInfo) ->
                      ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderObjectInfoAll rendering sdl camera body_data info_render object_info
  = case numericProperties object_info of
      Nothing => pure ()
      Just dict => let properties = toList dict in
          renderNumericProperties rendering sdl camera body_data info_render properties

renderObject : SDL m => Rendering m => GameIO m =>
               (rendering : Var) ->
               (sdl : Var) ->
               (camera : Camera) ->
               (id : ObjectId) ->
               (body_data : BodyData) ->
               (rendering_description : RenderMethod) ->
               ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderObject rendering sdl camera id body_data Invisible = pure ()
renderObject rendering sdl camera id body_data (Tiled ref tileDims@(w, h) howMany@(nx, ny))
  = let tileDimsFull = dimToScreen camera (2 `scale` tileDims)
        topleft = position body_data - (cast nx * w, - cast ny * h)
        initial = positionToScreen camera topleft
        in tile sdl camera ref initial tileDimsFull (nx, ny)
renderObject rendering sdl camera id body_data (ColoredRect color dims)
  = let rect = getRect camera (position body_data) dims
        in filledRect sdl rect color
renderObject rendering sdl camera id body_data (ColoredCircle color radius) = pure ()
renderObject rendering sdl camera id body_data (Single ref dims)
  = let rect = getRect camera (position body_data) dims
        in drawWholeCenter sdl ref rect (getDegAngle body_data) (getFlip' body_data)
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
      lift $ log $ "renderLayer: no body data for " ++ id
      renderLayer rendering sdl xs
    Just body_data => with ST do
      camera <- queryPRendering rendering camera
      renderObject rendering sdl camera id body_data $ method render_description
      objects_info <- queryPRendering rendering info
      case (lookup id objects_info, info render_description) of
        (Just object_info, Just info_render) => with ST do
          renderObjectInfoAll rendering sdl camera body_data info_render object_info
          renderLayer rendering sdl xs
        _ => renderLayer rendering sdl xs

          -- case getScreenDimensions camera body_data render_description of
          --   Nothing => lift $ log $ "couldn't get screen dimensions for " ++ id
          --
          --   -- HERE
          --   -- the yd parameter for info changes, but it should be constant
          --   -- probably add as a render parameter
          --
          --   Just screenDimensions => with ST do

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
  camera <- queryPRendering rendering camera
  background <- queryPRendering rendering background
  renderBackground sdl camera background
  renderLayers rendering sdl !(queryPRendering rendering layerList)

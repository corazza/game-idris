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
import Dynamics.BodyData
import Dynamics.DynamicsControl
import Dynamics.MoveDirection
import Dynamics.DynamicsEvent
import Descriptions.ObjectDescription
import Descriptions.AnimationDescription
import Descriptions.MapDescription
import Descriptions.Color
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.NumericPropertyRender
import Descriptions.ItemDescription

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

  applyAnimationUpdate : (rendering : Var) ->
                         (animationUpdates : AnimationUpdate) ->
                         ST m () [rendering ::: SRendering]
  applyAnimationUpdates : (rendering : Var) ->
                          (animationUpdates : List AnimationUpdate) ->
                          ST m () [rendering ::: SRendering]
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
    update rendering . setAnimationStateStarted id
  runCommand rendering (Start (Movement Right) id) = ticks >>=
    update rendering . setAnimationStateStarted id
  runCommand rendering (Start (Movement Up) id) = pure ()
  runCommand rendering (Start (Movement Down) id) = pure ()
  runCommand rendering (Start (Attack x) id) = pure ()
  runCommand rendering (Start Walk id) = ticks >>=
    update rendering . setAnimationStateStarted id
  runCommand rendering (Stop (Movement Left) id) = ticks >>=
    update rendering . setAnimationStateStarted id
  runCommand rendering (Stop (Movement Right) id) = ticks >>=
    update rendering . setAnimationStateStarted id
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

  applyAnimationUpdate rendering (MkAnimationUpdate id name)
    = update rendering $ prenderingUpdateAnimationStateName id name

  applyAnimationUpdates rendering [] = pure ()
  applyAnimationUpdates rendering (x::xs) = applyAnimationUpdate rendering x >>=
    const (applyAnimationUpdates rendering xs)

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
-- getFlip : Int -> AnimationDescription -> Int
-- getFlip orig animation_description
--   = case (facingRight animation_description, orig) of
--       (True, 2) => 2
--       (True, 0) => 0
--       (False, 2) => 0
--       (False, 0) => 2

getFlip' : BodyData -> Int
getFlip' body_data = case forceDirection body_data of
  Leftward => 2
  Rightward => 0

correctFacing : Bool -> Int -> Int
correctFacing True x = x
correctFacing False 2 = 0
correctFacing False 0 = 2
correctFacing _ x = x

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

executeMethodInfoAll : SDL m => Rendering m => GameIO m =>
                      (rendering : Var) ->
                      (sdl : Var) ->
                      (camera : Camera) ->
                      (body_data : BodyData) ->
                      (info_render : InfoRenderParameters) ->
                      (object_info : ObjectInfo) ->
                      ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
executeMethodInfoAll rendering sdl camera body_data info_render object_info
  = case numericProperties object_info of
      Nothing => pure ()
      Just dict => let properties = toList dict in
          renderNumericProperties rendering sdl camera body_data info_render properties

flipOffset : Int -> Vector2D -> Vector2D
flipOffset 2 (x, y) = (-x, y)
flipOffset _ (x, y) = (x, y)

mutual
  renderEquipment : SDL m => Rendering m => GameIO m =>
                    (rendering : Var) ->
                    (sdl : Var) ->
                    (camera : Camera) ->
                    (eq_id : ObjectId) ->
                    (position : Vector2D) ->
                    (offset : Vector2D) ->
                    (angle : Double) ->
                    (flip : Int) ->
                    (carry_dims : Vector2D) ->
                    (ref : ContentReference) ->
                    ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
  renderEquipment rendering sdl camera eq_id position offset angle flip carry_dims ref
    = with ST do
        preload <- queryPRendering rendering preload
        case getItemDescription ref preload of
          Left e => lift $ log $
            "couldn't get item description for " ++ ref ++ " (executeMethod)"
          Right item_desc => case attackRender item_desc of
            Nothing => pure ()
            Just method =>
              let offset' = flipOffset flip (offset - item_offset item_desc)
                  position' = position + offset'
                  in executeMethod
                    rendering sdl camera eq_id position' angle flip method

  executeMethod : SDL m => Rendering m => GameIO m =>
                 (rendering : Var) ->
                 (sdl : Var) ->
                 (camera : Camera) ->
                 (id : ObjectId) ->
                 (position : Vector2D) ->
                 (angle : Double) ->
                 (flip : Int) ->
                 (rendering_description : RenderMethod) ->
                 ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
  executeMethod rendering sdl camera id position angle flip Invisible = pure ()
  executeMethod rendering sdl camera id position angle flip (Tiled ref tileDims@(w, h) howMany@(nx, ny))
    = let tileDimsFull = dimToScreen camera (2 `scale` tileDims)
          topleft = position - (cast nx * w, - cast ny * h)
          initial = positionToScreen camera topleft
          in tile sdl camera ref initial tileDimsFull (nx, ny)
  executeMethod rendering sdl camera id position angle flip (ColoredRect color dims)
    = let rect = getRect camera position dims
          in filledRect sdl rect color
  executeMethod rendering sdl camera id position angle flip (ColoredCircle color radius) = pure ()
  executeMethod rendering sdl camera id position angle flip (Single ref dims facingRight)
    = let rect = getRect camera position dims
          in drawWholeCenter sdl ref rect angle (correctFacing facingRight flip)
  executeMethod rendering sdl camera id position angle flip (Animated state_dict) =
    case !(queryPRendering rendering (getAnimationState id)) of
      Nothing => pure ()
      Just (MkAnimationState name started attackShowing) =>
        case lookup name state_dict of
          Nothing => pure ()
          Just aparams => with ST do
            preload <- queryPRendering rendering preload
            let animation_ref = ref aparams
            case getAnimationDescription animation_ref preload of
              Left e => lift $ log $ missingDescription animation_ref id e
              Right animation_description =>
                let src = getSrc !ticks (fps aparams) animation_description
                    dimensions' = dimensions aparams
                    dst = getRect camera position dimensions'
                    sheet = sheet animation_description
                    flip = correctFacing (facingRight animation_description) flip
                    in drawCenter sdl sheet src dst angle flip

renderEquipment' : SDL m => Rendering m => GameIO m =>
                   (rendering : Var) ->
                   (sdl : Var) ->
                   (camera : Camera) ->
                   (id : ObjectId) ->
                   (position : Vector2D) ->
                   (angle : Double) ->
                   (flip : Int) ->
                   (desc : RenderDescription) ->
                   ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderEquipment' rendering sdl camera id position angle flip desc
  = case method desc of
      Animated state_dict =>
        case !(queryPRendering rendering (getAnimationState id)) of
          Nothing => pure ()
          Just (MkAnimationState name started attackShowing) =>
            case lookup name state_dict of
              Nothing => pure ()
              Just aparams => with ST do
                case attackShowing of
                  Nothing => pure ()
                  Just ref => with ST do
                    let eq_id = id ++ "_equipment"
                    updatePRendering rendering $
                      addInitialAnimationState eq_id !ticks
                    renderEquipment rendering sdl camera eq_id position
                                    (getHandsOffset aparams) 0 flip
                                    (dimensions aparams) ref
      _ => pure ()

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
      let position' = position body_data
      let angle = getDegAngle body_data
      let flip = getFlip' body_data
      executeMethod rendering sdl camera id position' angle flip $ method render_description
      renderEquipment' rendering sdl camera id position' angle flip render_description
      objects_info <- queryPRendering rendering info
      case (lookup id objects_info, info render_description) of
        (Just object_info, Just info_render) => with ST do
          executeMethodInfoAll rendering sdl camera body_data info_render object_info
          renderLayer rendering sdl xs
        _ => renderLayer rendering sdl xs

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

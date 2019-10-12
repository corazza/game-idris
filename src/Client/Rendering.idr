module Client.Rendering

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D
import Graphics.SDL2

import Client.Rendering.Camera
import Client.Rendering.PRendering
import Client.Rendering.Info
import Client.Rendering.RenderMethods
import Client.Rendering.Layers
import Client.Rendering.PositionData
import Client.Rendering.AnimationState
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
                   PreloadResults ->
                   ST m Var [add SRendering]
  endRendering : (rendering : Var) -> ST m () [remove rendering SRendering]

  queryPRendering : (rendering : Var) ->
                    (q : PRendering -> a) ->
                    ST m a [rendering ::: SRendering]
  updatePRendering : (rendering : Var) ->
                     (f : PRendering -> PRendering) ->
                     ST m () [rendering ::: SRendering]

  setBackground : (rendering : Var) -> Background -> ST m () [rendering ::: SRendering]

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
  loadStatic : (rendering : Var) ->
               MapDescription ->
               ST m (Checked (List (StaticCreation, ObjectDescription)))
                    [rendering ::: SRendering]
  private
  addStatic : (rendering : Var) ->
              List (StaticCreation, ObjectDescription) ->
              ST m () [rendering ::: SRendering]
  loadMap : (rendering : Var) -> MapDescription -> ST m () [rendering ::: SRendering]

  private
  initAnimation : (rendering : Var) ->
                  (id : ObjectId) ->
                  (desc : Maybe RenderMethod) ->
                  ST m () [rendering ::: SRendering]

  private
  getAnimationStateQ : (rendering : Var) ->
                       ST m (ObjectId -> Maybe AnimationState) [rendering ::: SRendering]

export
(GameIO m, SDL m) => Rendering m where
  SRendering = State PRendering

  startRendering settings preload
    = new $ prenderingInitial settings preload

  endRendering rendering = delete rendering

  queryPRendering rendering q = read rendering >>= pure . q
  updatePRendering rendering f = update rendering f

  setBackground rendering background
    = updatePRendering rendering $ setBackground background

  getCamera rendering = queryPRendering rendering camera

  addObject rendering id desc = case render desc of
    Nothing => pure ()
    Just render_desc => with ST do
      update rendering $ updateLayers $ addToLayer id render_desc
      initAnimation rendering id $ method render_desc
      case rules desc of
        Nothing => pure ()
        Just rules_desc => update rendering $ addInfo id rules_desc

  initAnimation rendering id (Just (Animated stateDict))
    = ticks >>= update rendering . addInitialAnimationState id
  initAnimation rendering id _ = pure ()

  removeObject rendering id = with ST do
    update rendering $ updateLayers $ removeFromLayers id
    update rendering $ removeAnimationState id

  loadStatic rendering map_description
    = queryPRendering rendering preload >>= pure . flip getStaticFromMap map_description

  addStatic rendering [] = pure ()
  addStatic rendering ((creation, desc)::xs)
    = addObject rendering (id creation) desc >>= const (addStatic rendering xs)

  loadMap rendering map_description = with ST do
    Right static <- loadStatic rendering map_description | Left e => with ST do
      lift $ log $ "client couldn't get static, error:\n" ++ e
    addStatic rendering static
    setBackground rendering $ background map_description

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

  updateBodyData rendering bodyData
    = let positionData = bodyDataToPositionData bodyData
          in update rendering $ (updateFollow . setPositionData positionData)

  follow rendering id = update rendering $ setFollowing id

  zoom rendering x =
    update rendering $ updateCamera $ zoomByFactor $ computeZoomFactor x -- * abs (cast x)

  getSettings rendering = with ST do
    update rendering refreshSettings
    queryPRendering rendering settings

  applyAnimationUpdate rendering (MkAnimationUpdate id name)
    = update rendering $ prenderingUpdateAnimationStateName id name

  applyAnimationUpdates rendering [] = pure ()
  applyAnimationUpdates rendering (x::xs) = applyAnimationUpdate rendering x >>=
    const (applyAnimationUpdates rendering xs)

  getAnimationStateQ rendering = with ST do
    animationStates' <- queryPRendering rendering animationStates
    pure $ flip lookup animationStates'

renderNumericProperty : SDL m => Rendering m => GameIO m =>
                        (rendering : Var) ->
                        (sdl : Var) ->
                        (camera : Camera) ->
                        (position_data : PositionData) ->
                        (info_render : InfoRenderParameters) ->
                        (info_id : NumericPropertyId) ->
                        (info : NumericPropertyInfo) ->
                        ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderNumericProperty rendering sdl camera position_data info_render info_id info
  = with ST do
      preload <- queryPRendering rendering preload
      case getNumPropRender info_id preload of
        Left e => lift $ log $ "renderNumericProperty fail, error: " ++ e
        Right render => with ST do
          let fullWidth = lengthToScreen camera (fullWidth render)
          let (object_x, object_y) = positionToScreen camera (position position_data)
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
                          (position_data : PositionData) ->
                          (info_render : InfoRenderParameters) ->
                          (properties : List (NumericPropertyId, NumericPropertyInfo)) ->
                          ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderNumericProperties rendering sdl camera position_data info_render [] = pure ()
renderNumericProperties rendering sdl camera position_data info_render ((id, info)::xs)
  = renderNumericProperty rendering sdl camera position_data info_render id info

executeMethodInfoAll : SDL m => Rendering m => GameIO m =>
                      (rendering : Var) ->
                      (sdl : Var) ->
                      (camera : Camera) ->
                      (position_data : PositionData) ->
                      (info_render : InfoRenderParameters) ->
                      (object_info : ObjectInfo) ->
                      ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
executeMethodInfoAll rendering sdl camera position_data info_render object_info
  = case numericProperties object_info of
      Nothing => pure ()
      Just dict => let properties = toList dict in
          renderNumericProperties rendering sdl camera position_data info_render properties

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
      Just (Animated state_dict) =>
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
                    preload' <- queryPRendering rendering preload
                    aq <- getAnimationStateQ rendering
                    renderEquipment aq preload' sdl camera eq_id position
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
  positionData <- queryPRendering rendering positionData
  case lookup id positionData of
    Nothing =>  with ST do
      lift $ log $ "renderLayer: no body data for " ++ id
      renderLayer rendering sdl xs
    Just position_data => with ST do
      camera <- queryPRendering rendering camera
      let position' = position position_data
      let angle' = angle position_data
      let flip' = flip position_data
      preload' <- queryPRendering rendering preload
      aq <- getAnimationStateQ rendering
      executeMethod aq preload' sdl camera id position' angle' flip' $ method render_description
      renderEquipment' rendering sdl camera id position' angle' flip' render_description
      objects_info <- queryPRendering rendering info
      case (lookup id objects_info, info render_description) of
        (Just object_info, Just info_render) => with ST do
          executeMethodInfoAll rendering sdl camera position_data info_render object_info
          renderLayer rendering sdl xs
        _ => renderLayer rendering sdl xs

renderLayers : SDL m => Rendering m => GameIO m =>
               (rendering : Var) ->
               (sdl : Var) ->
               (layers : LayerList) ->
               ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderLayers rendering sdl [] = pure ()
renderLayers rendering sdl (layer::xs)
  = renderLayer rendering sdl layer >>=
      const (renderLayers rendering sdl xs)

export
renderBackground' : (SDL m, Rendering m, GameIO m) =>
                    (rendering : Var) ->
                    (sdl : Var) ->
                    (camera : Camera) ->
                    ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
renderBackground' rendering sdl camera = with ST do
  Just background <- queryPRendering rendering background | pure ()
  renderBackground sdl camera background

export
render : (SDL m, Rendering m, GameIO m) =>
         (rendering : Var) ->
         (sdl : Var) ->
         ST m () [rendering ::: SRendering {m}, sdl ::: SSDL {m}]
render rendering sdl = with ST do
  camera <- queryPRendering rendering camera
  renderBackground' rendering sdl camera
  renderLayers rendering sdl !(queryPRendering rendering $ queryLayers layerList)

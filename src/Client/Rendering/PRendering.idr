module Client.Rendering.PRendering

import Control.ST
import Control.ST.ImplicitCall

import Client.Rendering.Camera
import Client.Rendering.Info
import Client.SDL
import Dynamics.BodyData
import GameIO
import Objects
import Descriptions
import JSONCache
import Settings
import Descriptions.MapDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RenderDescription

public export
record AnimationState where
  constructor MkAnimationState
  name : String
  started : Int
  attackShowing : Maybe ContentReference

setStarted : Int -> AnimationState -> AnimationState
setStarted ticks = record { started = ticks }

setAttackShowing : ContentReference -> AnimationState -> AnimationState
setAttackShowing ref = record { attackShowing = Just ref }
unsetAttackShowing : AnimationState -> AnimationState
unsetAttackShowing = record { attackShowing = Nothing }

setName : String -> AnimationState -> AnimationState
setName name' = record { name = name' }

public export
record PRendering where
  constructor MkPRendering
  background : Background
  layers : Dict Nat (Objects RenderDescription)
  animationStates : Objects AnimationState
  camera : Camera
  following : Maybe ObjectId
  bodyData : Objects BodyData
  preload : PreloadResults
  info : Objects ObjectInfo
  settings : RenderingSettings

export
refreshSettings : PRendering -> PRendering
refreshSettings prendering
  = let newCameraSettings = toSettings $ camera prendering
        in record { settings $= setCameraSettings newCameraSettings } prendering

export
prenderingInitial : RenderingSettings ->
                    Background ->
                    PreloadResults ->
                    PRendering
prenderingInitial settings background preload
  = let camera = fromSettings $ cameraSettings settings
        in MkPRendering background empty empty camera Nothing empty preload empty settings

export
addToLayer : (id : ObjectId) ->
             (desc : RenderDescription) ->
             PRendering -> PRendering
addToLayer id desc prendering = let layer = fromMaybe 0 (layer desc) in
  case hasKey layer (layers prendering) of
    False => let layerDict = the (Objects RenderDescription) $ insert id desc empty
                 in record { layers $= insert layer layerDict } prendering
    True => record { layers $= update layer (insert id desc) } prendering

export
removeFromLayer : (id : ObjectId) -> (layer : Nat) -> PRendering -> PRendering
removeFromLayer id layer = record { layers $= update layer (removeObject id) }

export
removeFromLayers : (id : ObjectId) -> PRendering -> PRendering
removeFromLayers id = record { layers $= map (removeObject id) }

export
layerList : PRendering -> List (List (ObjectId, RenderDescription))
layerList prendering
  = let layers' : List (Nat, Objects RenderDescription) = toList (layers prendering)
        sorted = sortBy compareLayer layers'
        in map (DDict.toList . snd) sorted where
           compareLayer : (Nat, Objects RenderDescription) ->
                          (Nat, Objects RenderDescription) ->
                          Ordering
           compareLayer (x, _) (y, _) = compare x y

export
getRenderingDescription : ObjectId -> PRendering -> Maybe RenderDescription
getRenderingDescription id
  = head' . catMaybes . map (DDict.lookup id) . map snd . toList . layers

export
getBodyData : ObjectId -> PRendering -> Maybe BodyData
getBodyData id = lookup id . bodyData

export
addInitialAnimationState : (id : ObjectId) -> (clock : Int) -> PRendering -> PRendering
addInitialAnimationState id clock
  = record { animationStates $= addObject id (MkAnimationState "resting" clock Nothing) }

export
getAnimationState : (id : ObjectId) -> PRendering -> Maybe AnimationState
getAnimationState id prendering = lookup id (animationStates prendering)

export
setAnimationStateStarted : (id : ObjectId) -> (ticks : Int) -> PRendering -> PRendering
setAnimationStateStarted id ticks
  = record { animationStates $= updateObject id (setStarted ticks) }

export
removeAnimationState : (id : ObjectId) -> PRendering -> PRendering
removeAnimationState id = record { animationStates $= removeObject id }

export
addInfo : (id : ObjectId) -> (desc : RulesDescription) -> PRendering -> PRendering
addInfo object_id desc = case objectInfoFromRulesDescription desc of
  Nothing => id
  Just object_info => record { info $= addObject object_id object_info }

export
prenderingUpdateNumericProperty : (object_id : ObjectId) ->
                                  (prop_id : NumericPropertyId) ->
                                  (current : Double) ->
                                  PRendering -> PRendering
prenderingUpdateNumericProperty object_id prop_id current
  = record { info $= updateObject object_id (updateNumericProperty prop_id current) }

export
prenderingUpdateAnimationState : (object_id : ObjectId) ->
                                 (f : AnimationState -> AnimationState) ->
                                 PRendering -> PRendering
prenderingUpdateAnimationState object_id f
  = record { animationStates $= updateObject object_id f }

export
prenderingSetAttackShowing : (object_id : ObjectId) ->
                             (ref : ContentReference) ->
                             PRendering -> PRendering
prenderingSetAttackShowing object_id ref
  = prenderingUpdateAnimationState object_id (setAttackShowing ref)
  -- = record { animationStates $= updateObject object_id (setAttackShowing ref) }

export
prenderingUnsetAttackShowing : (object_id : ObjectId) ->
                               PRendering -> PRendering
prenderingUnsetAttackShowing object_id
  = prenderingUpdateAnimationState object_id unsetAttackShowing
  -- = record { animationStates $= updateObject object_id unsetAttackShowing }

export
prenderingUpdateAnimationStateName : (object_id : ObjectId) ->
                                     (name : String) ->
                                     PRendering -> PRendering
prenderingUpdateAnimationStateName object_id
  = prenderingUpdateAnimationState object_id . setName

export
setBodyData : (bodyData : Objects BodyData) -> PRendering -> PRendering
setBodyData bodyData = record { bodyData = bodyData }

export
setFollowing : ObjectId -> PRendering -> PRendering
setFollowing id = record { following = Just id }

export
unsetFollowing : PRendering -> PRendering
unsetFollowing = record { following = Nothing }

export
setCameraOn : ObjectId -> PRendering -> PRendering
setCameraOn id prendering = case lookup id (bodyData prendering) of
  Nothing => prendering
  Just body_data => record { camera $= translate (position body_data) } prendering

export
updateFollow : PRendering -> PRendering
updateFollow prendering = case following prendering of
  Nothing => prendering
  Just id => setCameraOn id prendering

export
updateCamera : (f : Camera -> Camera) -> PRendering -> PRendering
updateCamera f = record { camera $= f }

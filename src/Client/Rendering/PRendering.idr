module Client.Rendering.PRendering

import Control.ST
import Control.ST.ImplicitCall

import Client.Rendering.Camera
import Client.Rendering.Info
import Client.Rendering.Layers
import Client.Rendering.PositionData
import Client.Rendering.AnimationState
import Client.SDL
import Dynamics.BodyData
import GameIO
import Objects
import JSONCache
import Settings
import Descriptions.MapDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RenderDescription

public export
record PRendering where
  constructor MkPRendering
  background : Maybe Background
  layers : Layers
  animationStates : Objects AnimationState
  camera : Camera
  following : Maybe ObjectId
  positionData : Objects PositionData
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
                    PreloadResults ->
                    PRendering
prenderingInitial settings preload
  = let camera = fromSettings $ cameraSettings settings
        in MkPRendering Nothing empty empty camera Nothing empty preload empty settings

export
setBackground : Background -> PRendering -> PRendering
setBackground background' = record { background = Just background' }

export
getPositionData : ObjectId -> PRendering -> Maybe PositionData
getPositionData id = lookup id . positionData

export
updateLayers : (f : Layers -> Layers) -> PRendering -> PRendering
updateLayers f = record { layers $= f }

export
queryLayers : (q : Layers -> a) -> PRendering -> a
queryLayers q = q . layers

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

export
prenderingUnsetAttackShowing : (object_id : ObjectId) ->
                               PRendering -> PRendering
prenderingUnsetAttackShowing object_id
  = prenderingUpdateAnimationState object_id unsetAttackShowing

export
prenderingUpdateAnimationStateName : (object_id : ObjectId) ->
                                     (name : String) ->
                                     PRendering -> PRendering
prenderingUpdateAnimationStateName object_id
  = prenderingUpdateAnimationState object_id . setName

export
setPositionData : (positionData : Objects PositionData) -> PRendering -> PRendering
setPositionData positionData' = record { positionData = positionData' }

export
setFollowing : ObjectId -> PRendering -> PRendering
setFollowing id = record { following = Just id }

export
unsetFollowing : PRendering -> PRendering
unsetFollowing = record { following = Nothing }

export
setCameraOn : ObjectId -> PRendering -> PRendering
setCameraOn id prendering = case lookup id (positionData prendering) of
  Nothing => prendering
  Just position_data => record {
    camera $= translate (position position_data) } prendering

export
updateFollow : PRendering -> PRendering
updateFollow prendering = case following prendering of
  Nothing => prendering
  Just id => setCameraOn id prendering

export
updateCamera : (f : Camera -> Camera) -> PRendering -> PRendering
updateCamera f = record { camera $= f }

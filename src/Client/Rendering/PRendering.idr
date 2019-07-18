module Client.Rendering.PRendering

import Control.ST
import Control.ST.ImplicitCall

import Client.Rendering.Camera
import Client.SDL
import Dynamics
import Dynamics.PDynamics
import GameIO
import Objects
import Descriptions
import JSONCache
import Descriptions.ObjectDescription.RenderDescription

public export
data AnimationState = MkAnimationState Int -- state, started (ms)

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

export
prenderingInitial : Background -> Camera -> PreloadResults -> PRendering
prenderingInitial background camera preload
  = MkPRendering background empty empty camera Nothing empty preload

export
addToLayer : (id : ObjectId) ->
             (desc : RenderDescription) ->
             (layer : Nat) ->
             PRendering -> PRendering
addToLayer id desc layer prendering = case hasKey layer (layers prendering) of
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
addInitialAnimationState : (id : ObjectId) -> (clock : Int) -> PRendering -> PRendering
addInitialAnimationState id clock
  = record { animationStates $= addObject id (MkAnimationState clock) }

export
getAnimationState : (id : ObjectId) -> PRendering -> Maybe AnimationState
getAnimationState id prendering = lookup id (animationStates prendering)

export
setAnimationState : (id : ObjectId) -> (state : AnimationState) -> PRendering -> PRendering
setAnimationState id state = record { animationStates $= updateObject id (const state) }

export
removeAnimationState : (id : ObjectId) -> PRendering -> PRendering
removeAnimationState id = record { animationStates $= removeObject id }

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
updateCamera : PRendering -> PRendering
updateCamera prendering = case following prendering of
  Nothing => prendering
  Just id => setCameraOn id prendering

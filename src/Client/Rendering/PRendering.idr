module Client.Rendering.PRendering

import Control.ST
import Control.ST.ImplicitCall

import Client.Rendering.Camera
import Client.SDL
import Dynamics
import GameIO
import Objects
import Descriptions
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

export
addToLayer : (id : ObjectId) ->
             (desc : RenderDescription) ->
             (layer : Nat) ->
             PRendering -> PRendering
addToLayer id desc layer = record { layers $= update layer (insert id desc) }

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
setAnimationState : (id : ObjectId) -> (state : AnimationState) -> PRendering -> PRendering
setAnimationState id state = record { animationStates $= updateObject id (const state) }

export
removeAnimationState : (id : ObjectId) -> PRendering -> PRendering
removeAnimationState id = record { animationStates $= removeObject id }

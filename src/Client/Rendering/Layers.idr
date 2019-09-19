module Client.Rendering.Layers

import Data.AVL.Dict

import GameIO
import Objects
import Descriptions.ObjectDescription.RenderDescription

public export
Layers : Type
Layers = Dict Nat (Objects RenderDescription)

public export
LayerList : Type
LayerList = List (List (ObjectId, RenderDescription))

export
addToLayer : (id : ObjectId) ->
             (desc : RenderDescription) ->
             Layers -> Layers
addToLayer id desc layers = let layer = fromMaybe 0 (layer desc) in
  case hasKey layer layers of
    False => let layerDict = the (Objects RenderDescription) $ insert id desc empty
                 in (insert layer layerDict) layers
    True => (update layer (insert id desc)) layers

export
removeFromLayer : (id : ObjectId) -> (layer : Nat) -> Layers -> Layers
removeFromLayer id layer = update layer $ removeObject id

export
removeFromLayers : (id : ObjectId) -> Layers -> Layers
removeFromLayers id = map $ removeObject id

export
layerList : Layers -> LayerList
layerList layers
  = let layers' : List (Nat, Objects RenderDescription) = toList layers
        sorted = sortBy compareLayer layers'
        in map (DDict.toList . snd) sorted where
           compareLayer : (Nat, Objects RenderDescription) ->
                          (Nat, Objects RenderDescription) ->
                          Ordering
           compareLayer (x, _) (y, _) = compare x y

export
getRenderingDescription : ObjectId -> Layers -> Maybe RenderDescription
getRenderingDescription id
  = head' . catMaybes . map (DDict.lookup id) . map snd . toList

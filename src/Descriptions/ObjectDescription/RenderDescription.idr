module Descriptions.ObjectDescription.RenderDescription

import Physics.Vector2D

import Descriptions.Color
import GameIO
import Exception

public export
record EquipmentRender where
  constructor MkEquipmentRender
  head : Vector2D
  hands : Vector2D
  legs : Vector2D

ObjectCaster EquipmentRender where
  objectCast dict = with Checked do
    head <- getVector "head" dict
    hands <- getVector "hands" dict
    legs <- getVector "legs" dict
    pure $ MkEquipmentRender head hands legs

public export
record AnimationParameters where
  constructor MkAnimationParameters
  ref : ContentReference
  dimensions : Vector2D
  fps : Double
  equipment : Maybe EquipmentRender
%name AnimationParameters animation_parameters

export
getHandsOffset : AnimationParameters -> Vector2D
getHandsOffset = fromMaybe nullVector . map hands . equipment

export
Show AnimationParameters where
  show (MkAnimationParameters ref dimensions fps equip)
    =  "{ ref: " ++ show ref
    ++ ", dimensions: " ++ show dimensions
    ++ ", fps: " ++ show fps
    ++ " }"

export
ObjectCaster AnimationParameters where
  objectCast dict = with Checked do
    animation <- getString "animation" dict
    dimensions <- getVector "dimensions" dict
    fps <- getDouble "fps" dict
    equip <- the (Checked (Maybe EquipmentRender)) $ getCastableMaybe "equip" dict
    pure $ MkAnimationParameters animation dimensions fps equip

public export
AnimationParametersDict : Type
AnimationParametersDict = Dict String AnimationParameters

public export
data RenderMethod = Invisible
                  | Tiled ContentReference Vector2D (Nat, Nat)
                  | ColoredCircle Color Double
                  | ColoredRect Color Vector2D
                  | Single ContentReference Vector2D Bool
                  | Animated AnimationParametersDict
%name RenderMethod render_description

export
getSingleAnimation : AnimationParametersDict -> Maybe (ContentReference, Vector2D)
getSingleAnimation = map refDimensions . head' . Dict.toList where
  refDimensions : (String, AnimationParameters) -> (ContentReference, Vector2D)
  refDimensions (stateName, params) = (ref params, dimensions params)

export
getSingleAnimation' : AnimationParametersDict -> Maybe AnimationParameters
getSingleAnimation' = map snd . head' . Dict.toList

export
Show RenderMethod where
  show Invisible = "invisible"
  show (Tiled ref tileDims repeat)
    =    "tiled with " ++ show ref
    ++ " (tileDims: " ++ show tileDims
    ++ ", repeated: " ++ show repeat
    ++  ")"
  show (ColoredCircle color radius) = "colored with " ++ show color
  show (ColoredRect color dims) = "colored with " ++ show color
  show (Single ref dims facingRight) = "single with " ++ ref ++ ", dims: " ++ show dims
  show (Animated x) = "animated ( " ++ show x ++ " )"

toParameters : (String, JSON) -> Checked (String, AnimationParameters)
toParameters (state, json) = case the (Checked AnimationParameters) (cast json) of
  Left e => fail e
  Right aparams => pure (state, aparams)

getAnimationStates : Dict String JSON -> Checked (Dict String AnimationParameters)
getAnimationStates dict = case lookup "states" dict of
  Nothing => fail "missing animation states"
  Just (JObject xs) => with Checked do
    let attempt = map toParameters xs
    -- aparams <- foldr toChecked (pure empty) attempt
    aparams <- catResults attempt
    pure $ fromList aparams
  _ => fail "animation states aren't JObject"

export
ObjectCaster RenderMethod where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "invisible" => pure Invisible
      "color" => with Checked do
        color <- getColor "color" dict
        case (hasKey "dimensions" dict, hasKey "radius" dict) of
          (True, True) => fail "dimensions and radius can't both be present for render method color"
          (True, False) => getVector "dimensions" dict >>= pure . ColoredRect color
          (False, True) => getDouble "radius" dict >>= pure . ColoredCircle color
          (False, False) => fail "either dimensions or radius must be present for render method color"
      "single" => with Checked do
        image <- getString "image" dict
        dimensions <- getVector "dimensions" dict
        facingRight <- getBoolOrDefault True "facingRight" dict
        pure $ Single image dimensions facingRight
      "tile" => with Checked do
        image <- getString "image" dict
        tileDims <- getVector "tileDims" dict
        (nx, ny) <- getIntPair "repeat" dict
        pure $ Tiled image tileDims (cast nx, cast ny)
      "animated" => getAnimationStates dict >>= pure . Animated
      _ => fail "render type must be of \"invisible\"|\"single\"|\"tile\"|\"animated\""

public export
record InfoRenderParameters where
  constructor MkInfoRenderParameters
  yd : Double

export
Show InfoRenderParameters where
  show info = "{ yd: " ++ show (yd info) ++ " }"

ObjectCaster InfoRenderParameters where
  objectCast dict = with Checked do
    yd <- getDouble "yd" dict
    pure $ MkInfoRenderParameters yd

public export
record RenderDescription where
  constructor MkRenderDescription
  method : RenderMethod
  info : Maybe InfoRenderParameters
  layer : Maybe Nat

export
Show RenderDescription where
  show rd
    =  "{ method: " ++ show (method rd)
    ++ ", info: " ++ show (info rd)
    ++ ", layer: " ++ show (layer rd)
    ++ " }"

export
ObjectCaster RenderDescription where
  objectCast dict = with Checked do
    method <- the (Checked RenderMethod) $ getCastable "method" dict
    info <- the (Checked (Maybe InfoRenderParameters)) $ getCastableMaybe "info" dict
    let layer = eitherToMaybe $ getInt "layer" dict
    pure $ MkRenderDescription method info (map cast layer)

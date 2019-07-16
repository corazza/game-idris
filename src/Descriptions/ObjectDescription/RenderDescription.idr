module Descriptions.ObjectDescription.RenderDescription

import Physics.Vector2D

import Descriptions.Color
import GameIO
import Exception

public export
record AnimationParameters where
  constructor MkAnimationParameters
  sheet : ResourceReference
  dimensions : Vector2D
  fps : Double
%name AnimationParameters animation_parameters

export
Show AnimationParameters where
  show (MkAnimationParameters sheet dimensions fps)
    =  "{ sheet: " ++ show sheet
    ++ ", dimensions: " ++ show dimensions
    ++ ", fps: " ++ show fps
    ++ " }"

export
ObjectCaster AnimationParameters where
  objectCast dict = with Checked do
    animation <- getString "animation" dict
    dimensions <- getVector "dimensions" dict
    speed <- getDouble "fps" dict
    pure $ MkAnimationParameters animation dimensions speed

public export
data RenderDescription = Invisible
                       | Tiled ResourceReference Vector2D (Nat, Nat)
                       | Colored Color
                       | Single ResourceReference Vector2D
                       | Animated (Dict String AnimationParameters)
%name RenderDescription render_description

export
Show RenderDescription where
  show Invisible = "invisible"
  show (Tiled ref tileDims repeat)
    =    "tiled with " ++ show ref
    ++ " (tileDims: " ++ show tileDims
    ++ ", repeated: " ++ show repeat
    ++  ")"
  show (Colored color) = "colored with " ++ show color
  show (Single ref dims) = "single with " ++ ref ++ ", dims: " ++ show dims
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
ObjectCaster RenderDescription where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "invisible" => pure Invisible
      "color" => getColor "color" dict >>= pure . Colored
      "single" => with Checked do
        image <- getString "image" dict
        dimensions <- getVector "dimensions" dict
        pure $ Single image dimensions
      "tile" => with Checked do
        image <- getString "image" dict
        tileDims <- getVector "tileDims" dict
        (nx, ny) <- getIntPair "repeat" dict
        pure $ Tiled image tileDims (cast nx, cast ny)
      "animated" => getAnimationStates dict >>= pure . Animated
      _ => fail "render type must be of \"invisible\"|\"single\"|\"tile\"|\"animated\""

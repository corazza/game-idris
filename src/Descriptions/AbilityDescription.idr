module Descriptions.AbilityDescription

import GameIO
import Exception
import Objects

public export
data AbilityDescription
  = Throw ContentReference Double -- ref, impulse
  | Melee Double Double -- range, damage

export
ObjectCaster AbilityDescription where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "throw" => with Checked do
        ref <- getString "ref" dict
        impulse <- getDouble "impulse" dict
        pure $ Throw ref impulse
      "melee" => with Checked do
        range <- getDouble "range" dict
        damage <- getDouble "damage" dict
        pure $ Melee range damage
      _ => fail "ability type must be of \"throw\"|\"melee\""

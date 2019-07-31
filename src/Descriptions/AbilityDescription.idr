module Descriptions.AbilityDescription

import GameIO
import Exception
import Objects

public export
data AbilityDescription
  = Throw ContentReference Double -- ref, impulse

export
ObjectCaster AbilityDescription where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "throw" => with Checked do
        ref <- getString "ref" dict
        impulse <- getDouble "impulse" dict
        pure $ Throw ref impulse
      _ => fail "ability type must be of \"throw\""

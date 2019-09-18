module Descriptions.AbilityDescription

import GameIO
import Exception
import Objects

public export
data AbilityEffect
  = Throw ContentReference Double -- ref, impulse
  | Melee Double Double -- range, damage

export
ObjectCaster AbilityEffect where
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

public export
record AbilityDescription where
  constructor MkAbilityDescription
  effect : AbilityEffect
  preSound : Maybe ContentReference
  postSound : Maybe ContentReference

export
ObjectCaster AbilityDescription where
  objectCast dict = with Checked do
    effect <- the (Checked AbilityEffect) $ getCastable "effect" dict
    preSound <- getStringMaybe "preSound" dict
    postSound <- getStringMaybe "postSound" dict
    pure $ MkAbilityDescription effect preSound postSound

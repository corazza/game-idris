module Descriptions.WallDescription

import Physics.Vector2D

import Descriptions.ObjectDescription.BodyDescription
import Descriptions.Color
import GameIO
import Exception

public export
data WallRenderDescription = InvisibleWall
                           | TiledWall ResourceReference Vector2D -- tiledims
                           | ColoredWall Color
%name WallRenderDescription wall_render_description

export
Show WallRenderDescription where
  show InvisibleWall = "invisible"
  show (TiledWall ref tileDims) = ref ++ " (" ++ show tileDims ++ ")"
  show (ColoredWall color) = "colored with " ++ show color

ObjectCaster WallRenderDescription where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "invisible" => pure InvisibleWall
      "color" => getColor "color" dict >>= pure . ColoredWall
      "tile" => with ST do
        image <- getString "image" dict
        tileDims <- getVector "tileDims" dict
        pure $ TiledWall image tileDims
      _ => fail "wall render description type must be of \"invisible\"|\"tile\"|\"color\""

public export
record WallDescription where
  constructor MkWallDescription
  name : String
  fixture_parameters : FixtureParameters
  render : WallRenderDescription
%name WallDescription wall_description

export
Show WallDescription where
  show (MkWallDescription name fixture_parameters render)
    =  "{ name: " ++ name
    ++ ", fixture_parameters: " ++ show fixture_parameters
    ++ ", render: " ++ show render
    ++ " }"

-- in objects, these are part of a fixture field, here they're a field on their own
ObjectCaster FixtureParameters where
  objectCast = getFixtureParameters

export
ObjectCaster WallDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    fixture_parameters <- the (Checked FixtureParameters) $ getCastable "fixtureParameters" dict
    render <- the (Checked WallRenderDescription) $ getCastable "render" dict
    pure $ MkWallDescription name fixture_parameters render

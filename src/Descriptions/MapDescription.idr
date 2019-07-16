module Descriptions.MapDescription

import Physics.Box2D

import Descriptions.ObjectDescription
import Descriptions.ObjectDescription.BodyDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.ControlDescription
import Descriptions.WallDescription
import Descriptions.Color
import GameIO
import Exception

public export -- loaded by server, received by client
record Creation where
  constructor MkCreation
  ref : ResourceReference
  position : Vector2D
  impulse : Maybe Vector2D
  angle : Maybe Double
%name Creation creation

export
Show Creation where
  show creation = ref creation ++ " at " ++ show (position creation)

ObjectCaster Creation where
  objectCast dict = with Checked do
    ref <- getString "ref" dict
    position <- getVector "position" dict
    case hasKey "angle" dict of
      False => pure $ MkCreation ref position Nothing Nothing
      True => getDouble "angle" dict >>= pure . MkCreation ref position Nothing . Just

export
creationBodyDescriptionToDefinition : Creation -> BodyDescription -> BodyDefinition
creationBodyDescriptionToDefinition creation desc = MkBodyDefinition
  (type desc) (position creation) (angle creation) (fixedRotation desc) (bullet desc)

public export
data WallData = Repeat (Nat, Nat)
              | Dimensions Vector2D
%name WallData wall_data

export
Show WallData where
  show (Repeat nxny) = "repeat " ++ show nxny
  show (Dimensions xy) = "dimensions " ++ show xy

getWallData : JSONDict -> Checked WallData
getWallData dict = case (hasKey "dimensions" dict, hasKey "repeat" dict) of
  (True, False) => getVector "dimensions" dict >>= pure . Dimensions
  (False, True) => getNatPair "repeat" dict >>= pure . Repeat
  (False, False) => fail "either \"dimensions\" or \"repeat\" fields have to be present"
  (True, False) => fail "\"dimensions\" and \"repeat\" fields can't both be present"

export
wallDescToObjectDesc : WallDescription -> WallData -> Checked ObjectDescription
wallDescToObjectDesc wall_desc wall_data = (with Checked do
  bodyDesc <- getBodyDesc
  renderDesc <- getRenderDesc
  pure $ MkObjectDescription (name wall_desc) bodyDesc renderDesc Nothing) where
    getShape : Checked Shape
    getShape = case wall_data of
      Repeat (nx, ny) => case render wall_desc of
        InvisibleWall => fail "InvisibleWall can't have repeat parameter"
        TiledWall ref (x, y) => pure $ Box (cast nx * x, cast ny * y)
        ColoredWall color => fail "ColoredWall can't have repeat parameter"
      Dimensions xy => pure $ Box xy

    getFixture : Checked FixtureDefinition
    getFixture = with Checked do
      shape <- getShape
      pure $ fixtureFromParametersShape (fixture_parameters wall_desc) shape

    getBodyDesc : Checked BodyDescription
    getBodyDesc = with Checked do
      fixture <- getFixture
      pure $ MkBodyDescription Static (Just True) (Just False) [fixture] empty

    getRenderDesc : Checked RenderDescription
    getRenderDesc = case render wall_desc of
      InvisibleWall => pure Invisible
      TiledWall ref tileDims => case wall_data of
        Repeat nxny => pure $ Tiled ref tileDims nxny
        Dimensions x => fail "TiledWall can't have dimensions parameter"
      ColoredWall color => pure $ Colored color

public export -- loaded both by server and client
record WallCreation where
  constructor MkWallCreation
  id : String
  ref : ResourceReference -- points to a wall description
  position : Vector2D
  angle : Maybe Double
  wall_data : WallData
%name WallCreation wall_creation

export
Show WallCreation where
  show (MkWallCreation id ref position angle wall_data)
    =  "{ id: " ++ id
    ++ ", ref: " ++ ref
    ++ ", position: " ++ show position
    ++ ", angle: " ++ show angle
    ++ ", wall_data: " ++ show wall_data
    ++ " }"

ObjectCaster WallCreation where
  objectCast dict = with Checked do
    id <- getString "id" dict
    ref <- getString "ref" dict
    position <- getVector "position" dict
    angle <- getDoubleMaybe "angle" dict
    wall_data <- getWallData dict
    pure $ MkWallCreation id ref position angle wall_data

export
wallCreationBodyDescriptionToCreation : WallCreation -> BodyDescription -> BodyDefinition
wallCreationBodyDescriptionToCreation creation desc = MkBodyDefinition
  Static (position creation) (angle creation) (Just True) (Just False)

public export
record Background where
  constructor MkBackground
  image : ResourceReference
  dimensions : Vector2D

export
Show Background where
  show (MkBackground image dimensions) = image ++ " " ++ show dimensions

ObjectCaster Background where
  objectCast dict = with Checked do
    image <- getString "image" dict
    dimensions <- getVector "dimensions" dict
    pure $ MkBackground image dimensions

public export
record MapDescription where
  constructor MkMapDescription
  name : String
  dimensions : Vector2D
  background : Background
  walls : List WallCreation
  creations : List Creation

export
Show MapDescription where
  show (MkMapDescription name dimensions background walls creations)
    =  "{ name: " ++ name
    ++ ", dimensions: " ++ show dimensions
    ++ ", background: " ++ show background
    ++ ", walls: " ++ show walls
    ++ ", creations: " ++ show creations
    ++ " }"

export
ObjectCaster MapDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    dimensions <- getVector "dimensions" dict
    background <- the (Checked Background) $ getCastable "background" dict
    wallsJSON <- getArray "walls" dict
    creationsJSON <- getArray "creations" dict
    walls <- catResults $ the (List (Checked WallCreation)) $ map cast wallsJSON
    creations <- catResults $ the (List (Checked Creation)) $ map cast creationsJSON
    pure $ MkMapDescription name dimensions background walls creations

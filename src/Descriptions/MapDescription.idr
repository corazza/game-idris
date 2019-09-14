module Descriptions.MapDescription

import Physics.Box2D

import Descriptions.ObjectDescription
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.BodyDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.ControlDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.WallDescription
import Descriptions.JointDescription
import Descriptions.Color
import GameIO
import Exception
import Timeline
import Timeline.Items
import Objects

public export -- loaded by server, received by client
record Creation where
  constructor MkCreation
  ref : ContentReference
  position : Vector2D
  impulse : Maybe Vector2D
  creator : Maybe ObjectId
  angle : Maybe Double
  behavior : Maybe BehaviorParameters
  id : Maybe ObjectId
%name Creation creation

export
Show Creation where
  show creation = ref creation ++ " at " ++ show (position creation)

getCreationAngle : JSONDict -> Checked (Maybe Double)
getCreationAngle dict = case hasKey "angle" dict of
  False => pure Nothing
  True => getDouble "angle" dict >>= pure . Just

ObjectCaster Creation where
  objectCast dict = with Checked do
    ref <- getString "ref" dict
    position <- getVector "position" dict
    angle <- getCreationAngle dict
    behavior <- the (Checked (Maybe BehaviorParameters)) $ getCastableMaybe "behavior" dict
    id <- getStringMaybe "id" dict
    pure $ MkCreation ref position Nothing Nothing angle behavior id

export
creationBodyDescriptionToDefinition : Creation -> BodyDescription -> BodyDefinition
creationBodyDescriptionToDefinition creation desc = MkBodyDefinition
  (type desc) (position creation) (angle creation) (fixedRotation desc) (bullet desc)

fromBehavior : BehaviorParameters -> RulesDescription
fromBehavior bp = MkRulesDescription Nothing Nothing Nothing (Just bp) Nothing

export
rulesDescFromCreation : Maybe RulesDescription -> Creation -> Maybe RulesDescription
rulesDescFromCreation Nothing creation = map fromBehavior (behavior creation)
rulesDescFromCreation (Just desc) creation = case behavior creation of
  Nothing => Just desc
  Just bp => Just (record { behavior = Just bp } desc)

export
forCharacter : Vector2D -> Character -> Creation
forCharacter position character
  = MkCreation (ref character) position Nothing Nothing Nothing Nothing Nothing

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
  method <- getRenderMethod
  let renderDesc = Just $ MkRenderDescription method Nothing Nothing
  pure $ MkObjectDescription (name wall_desc) bodyDesc renderDesc Nothing Nothing) where
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
      pure $ MkBodyDescription
        Static (Just True) (Just False) [fixture] empty Nothing Nothing Nothing

    getRenderMethod : Checked RenderMethod
    getRenderMethod = case render wall_desc of
      InvisibleWall => pure Invisible
      TiledWall ref tileDims => case wall_data of
        Repeat nxny => pure $ Tiled ref tileDims nxny
        Dimensions x => fail "TiledWall can't have dimensions parameter"
      ColoredWall color => fail "colored walls not implemented"

public export -- loaded both by server and client
record WallCreation where
  constructor MkWallCreation
  id : String
  ref : ContentReference -- points to a wall description
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
  image : ContentReference
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
  spawn : Vector2D
  background : Background
  walls : List WallCreation
  creations : List Creation
  joints : List JointDescription

export
Show MapDescription where
  show (MkMapDescription name dimensions spawn background walls creations joints)
    =  "{ name: " ++ name
    ++ ", dimensions: " ++ show dimensions
    ++ ", spawn: " ++ show spawn
    ++ ", background: " ++ show background
    ++ ", walls: " ++ show walls
    ++ ", creations: " ++ show creations
    ++ " }"

export
ObjectCaster MapDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    dimensions <- getVector "dimensions" dict
    spawn <- getVector "spawn" dict
    background <- the (Checked Background) $ getCastable "background" dict
    wallsJSON <- getArray "walls" dict
    creationsJSON <- getArray "creations" dict
    jointsJSON <- getArray "joints" dict -- TODO maybe
    walls <- catResults $ the (List (Checked WallCreation)) $ map cast wallsJSON
    creations <- catResults $ the (List (Checked Creation)) $ map cast creationsJSON
    joints <- catResults $ the (List (Checked JointDescription)) $ map cast jointsJSON
    pure $ MkMapDescription name dimensions spawn background walls creations joints

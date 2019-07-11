module Descriptors

import Language.JSON
import Control.ST
import Data.AVL.Dict
import Data.AVL.Set

import Physics.Box2D
import Physics.Vector2D
import Resources
import GameIO
import Common
import Exception

public export
data FlipState = None | Vertical | Horizontal | Both

export
Show FlipState where
  show None = "none"
  show Vertical = "vertical"
  show Horizontal = "horizontal"
  show Both = "both"

getFlipState : Dict String JSON -> Checked FlipState
getFlipState dict = case lookup "flip" dict of
  Nothing => pure None
  Just (JString x) => case x of
    "none" => pure None
    "vertical" => pure Vertical
    "horizontal" => pure Horizontal
    "both" => pure Both
    _ => fail $ "flip must be of " ++ "\"none\"|\"vertical\"|\"horizontal\"|\"both\""

-- TODO preload, should be : Animation and not : ResourceReference, when the
-- resource can be loaded immediately
public export
record AnimationParameters where
  constructor MkAnimationParameters
  animation : ResourceReference
  dimensions : Vector2D
  speed : Double
  -- flip : FlipState -- 0 no flip, 1 vertical, 2 horizontal, 3 both

export
Show AnimationParameters where
  show (MkAnimationParameters animation dimensions speed)
    = show animation ++ ", " ++ show dimensions ++  ", " ++ show speed

ObjectCaster AnimationParameters where
  objectCast dict = with Checked do
    animation <- getString "animation" dict
    dimensions <- getVector "dimensions" dict
    speed <- getDouble "speed" dict
    pure $ MkAnimationParameters animation dimensions speed

public export
data IncompleteRenderDescriptor
  = DrawBox ResourceReference Vector2D
  | TileWith ResourceReference Vector2D
  | Animated (Dict String AnimationParameters)
  | Invisible

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
    aparams <- listCheckedtoCheckedList attempt
    pure $ fromList aparams
  _ => fail "animation states aren't JObject"

ObjectCaster IncompleteRenderDescriptor where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "invisible" => pure Invisible
      "single" => with Checked do
        image <- getString "image" dict
        dimensions <- getVector "dimensions" dict
        pure $ DrawBox image dimensions
      "tile" => with Checked do
        image <- getString "image" dict
        tileDims <- getVector "tileDims" dict
        pure $ TileWith image tileDims
      "animated" => getAnimationStates dict >>= pure . Animated
      _ => fail "render type must be of \"invisible\"|\"single\"|\"tile\""

ObjectCaster Shape where
  objectCast dict = case lookup "type" dict of
    Just (JString "circle") => getDouble "radius" dict >>= pure . Circle
    Just (JString "box") => getVector "dimensions" dict >>= pure . Box
    Just (JString "polygon") => ?shapeDescriptorPolygon
    _ => fail "shape type must be of \"circle\"|\"box\"|\"polygon\""

public export
record FixtureParameters where
  constructor MkFixtureParameters
  offset : Maybe Vector2D
  angle : Maybe Double
  density : Maybe Double
  friction : Maybe Double
  restitution : Maybe Double
%name FixtureParameters params

-- gotten from the inside
getFixtureParameters : Dict String JSON -> FixtureParameters
getFixtureParameters dict
  = let offset = eitherToMaybe $ getVector "offset" dict
        angle = eitherToMaybe $ getDouble "angle" dict
        density = eitherToMaybe $ getDouble "density" dict
        friction = eitherToMaybe $ getDouble "friction" dict
        restitution = eitherToMaybe $ getDouble "restitution" dict in
          MkFixtureParameters offset angle density friction restitution


public export
data ShapedFixtureDescriptor = MkShapedFixtureDescriptor Shape FixtureParameters
public export
data ImmaterialFixtureDescriptor = MkImmaterialFixtureDescriptor FixtureParameters

ObjectCaster ShapedFixtureDescriptor where
  objectCast dict = with Checked do
    shape <- the (Checked Shape) $ getCastable "shape" dict
    pure $ MkShapedFixtureDescriptor shape (getFixtureParameters dict)

ObjectCaster ImmaterialFixtureDescriptor where
  objectCast dict = pure $ MkImmaterialFixtureDescriptor (getFixtureParameters dict)

Cast String (Checked BodyType) where
  cast "static" = pure Static
  cast "dynamic" = pure Dynamic
  cast "kinematic" = pure Kinematic
  cast x = fail $
    "body type must be of \"static\"|\"dynamic\"|\"kinematic\", not \"" ++ x ++ "\""

public export
data PhysicsEffect = Drag Double Vector2D

ObjectCaster PhysicsEffect where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "drag" => with Checked do
        factor <- getDouble "factor" dict
        offset <- getVector "offset" dict
        pure $ Drag factor offset
      _ => fail "effect type must be of \"drag\""

getPhysicsEffects : Dict String JSON -> Checked (List PhysicsEffect)
getPhysicsEffects dict = case (hasKey "physicsEffect" dict,  hasKey "physicsEffects" dict) of
  (True, False) => with Checked do
    effect <- the (Checked PhysicsEffect) $ getCastable "physicsEffect" dict
    pure [effect]
  (False, True) => with Checked do
    effects' <- getArray "physicsEffects" dict
    pure $ the (List PhysicsEffect) $ catMaybes $ map (eitherToMaybe . cast) effects'
  (False, False) => pure empty
  _ => fail "\"physicsEffect and physicsEffects can't both be present"

-- position and angle come from creation
public export
record BodyDescriptor where
  constructor MkBodyDescriptor
  type : BodyType
  fixedRotation : Maybe Bool
  bullet : Maybe Bool
  effects : List PhysicsEffect
  fixtures : Either ImmaterialFixtureDescriptor (List ShapedFixtureDescriptor) -- TODO prove not empty
%name BodyDescriptor desc

ObjectCaster BodyDescriptor where
  objectCast dict = with Checked do
    typeString <- getString "type" dict
    type <- the (Checked BodyType) (cast typeString)
    let fixedRotation = eitherToMaybe $ getBool "fixedRotation" dict
    let bullet = eitherToMaybe $ getBool "bullet" dict
    effects <- getPhysicsEffects dict
    let desc = MkBodyDescriptor type fixedRotation bullet effects
    case hasKey "immaterial" dict of
      True => with Checked do
        fixture <- the (Checked ImmaterialFixtureDescriptor) $ getCastable "immaterial" dict
        pure $ desc $ Left fixture
      False => case hasKey "fixture" dict of
            True => with Checked do
              fixture <- the (Checked ShapedFixtureDescriptor) $ getCastable "fixture" dict
              pure $ desc $ Right [fixture]
            False => with Checked do
              fixtures' <- getArray "fixtures" dict
              pure $ desc $ Right $ the (List ShapedFixtureDescriptor) $
                catMaybes $ map (eitherToMaybe . cast) fixtures'

public export -- additional parameters supplied later
data ScriptDescriptor = Create ResourceReference Double

ObjectCaster ScriptDescriptor where
  objectCast dict = case lookup "type" dict of
    Just (JString "create") => with Checked do
      ref <- getString "ref" dict
      impulse <- getDouble "impulse" dict
      pure $ Create ref impulse
    _ => fail "script type must be of \"create\""

Show ScriptDescriptor where
  show (Create ref impulse) = "create " ++ ref ++ ", " ++ show impulse

-- idk why `map cast (lookup "attack" dict)` doesn't work
getScript : String -> Dict String JSON -> Checked ScriptDescriptor
getScript name dict = case lookup name dict of
  Nothing => fail "no script"
  Just x => cast x

public export
record AIParameters where
  constructor MkAIParameters
  intParameters : Dict String Int
  doubleParameters : Dict String Double
  stringlistParameters : Dict String (List String)
%name AIParameters ai_parameters

export
Show AIParameters where
  show ai_parameters =  "int parameters: " ++ show (intParameters ai_parameters)
                     ++ ", " ++ show (stringlistParameters ai_parameters)

allToInt : (String, JSON) -> Checked (String, Int)
allToInt (name, JNumber x) = pure (name, cast x)
allToInt (name, _) = fail $ name ++ " not a number"

allToDouble : (String, JSON) -> Checked (String, Double)
allToDouble (name, JNumber x) = pure (name, x)
allToDouble (name, _) = fail $ name ++ " not a number"

allToStringlist : (String, JSON) -> Checked (String, List String)
allToStringlist (name, JArray xs)
  = listCheckedtoCheckedList (map jsonToString xs) >>= pure . MkPair name
allToStringlist (name, _) = fail $ name ++ " not an array"

getParameterType : (name : String) ->
                   (f : (String, JSON) -> Checked (String, a)) ->
                   (dict : Dict String JSON) ->
                   Checked (Dict String a)
getParameterType name f dict = case lookup name dict of
  Nothing => pure empty
  Just (JObject xs) => listCheckedtoCheckedList (map f xs) >>= pure . fromList
  _ => fail $ name ++ " must be JObject"

ObjectCaster AIParameters where
  objectCast dict = with Checked do
    intParameters <- getParameterType "int_parameters" allToInt dict
    doubleParameters <- getParameterType "double_parameters" allToDouble dict
    stringlistParameters <- getParameterType "stringlist_parameters" allToStringlist dict
    pure $ MkAIParameters intParameters doubleParameters stringlistParameters

public export
record ControlDescriptor where
  constructor MkControlDescriptor
  speed : Double
  jump : Double
  ai : Maybe ResourceReference
  ai_parameters : Maybe AIParameters
  -- attack : Maybe ScriptDescriptor
%name ControlDescriptor cdesc

export
Show ControlDescriptor where
  show (MkControlDescriptor speed jump ai ai_parameters)
  =  "{ speed: " ++ show speed
    ++ ", jump: " ++ show jump
    ++ ", ai: " ++ show ai
    ++ ", ai_parameters: " ++ show ai_parameters
    ++ " }"

ObjectCaster ControlDescriptor where
  objectCast dict = with Maybe do
    speed <- getDouble "speed" dict
    jump <- getDouble "jump" dict
    let ai = eitherToMaybe $ getString "ai" dict
    ai_parameters <- the (Checked (Maybe AIParameters)) $ getCastableMaybe "ai_parameters" dict
    pure $ MkControlDescriptor speed jump ai ai_parameters

getControl : Maybe JSON -> Checked (Maybe ControlDescriptor)
getControl Nothing = pure Nothing
getControl (Just x) = the (Checked ControlDescriptor) (cast x) >>= pure . Just

Cast String (Checked ObjectTag) where
  cast "spawn" = pure Spawn
  cast "projectile" = pure Projectile
  cast _ = fail "tag must be of \"spawn\" | \"projectile\""

getTags : Dict String JSON -> Checked (Set ObjectTag)
getTags dict = getStrings "tags" dict >>= catResults . map cast >>= pure . fromList

getTags' : Dict String JSON -> Set ObjectTag
getTags' dict = case getTags dict of
  Left e => empty
  Right s => s

public export
record ObjectDescriptor where
  constructor MkObjectDescriptor
  name : String
  bodyDescription : BodyDescriptor
  renderDescription : IncompleteRenderDescriptor
  tags : Set ObjectTag
  health : Maybe Double
  control : Maybe ControlDescriptor
  attack : Maybe ScriptDescriptor
%name ObjectDescriptor desc

-- TODO fix show implementations in this file

export
Show ObjectDescriptor where
  show (MkObjectDescriptor name bodyDescription renderDescription tags health control attack)
      =  "{ name: " ++ name
      ++ ", health: " ++ show health
      ++ ", control: " ++ show control
      ++ ", attack: " ++ show attack
      ++ " }"

-- TODO getters need checked alternatives
ObjectCaster ObjectDescriptor where
  objectCast dict = with Checked do
    name <- getString "name" dict
    box2d <- the (Checked BodyDescriptor) $ getCastable "body" dict
    render <- the (Checked IncompleteRenderDescriptor) $ getCastable "render" dict
    let control = the (Checked ControlDescriptor) $ getCastable "control" dict
    pure $ MkObjectDescriptor name box2d render (getTags' dict)
                              (eitherToMaybe $ getDouble "health" dict)
                              (eitherToMaybe control)
                              (eitherToMaybe $ getScript "attack" dict)

public export
data CreationData = BoxData (Maybe Vector2D) -- impulse
                  | WallData (Int, Int) -- number of tiles (x, y)
                  | InvisibleWallData Vector2D -- dimensions

export
impulseOnCreation : CreationData -> Vector2D
impulseOnCreation (BoxData (Just x)) = x
impulseOnCreation _ = nullVector

-- collects a flat description of a scene object creation for later processing
public export
record Creation where
  constructor MkCreation
  id : Maybe String
  ref : ResourceReference
  position : Vector2D
  angle : Double
  tags : Set ObjectTag
  creationData : CreationData
  creator : Maybe ObjectId
%name Creation creation

getId : Dict String JSON -> Maybe String
getId dict = with Maybe do
  JString id <- lookup "id" dict | Nothing
  Just id

extractWallData : Dict String JSON -> Maybe CreationData
extractWallData dict = let repeat = lookup "repeat" dict
                           dimensions = lookup "dimensions" dict in
            case (repeat, dimensions) of
              (Just (JArray [JNumber xn, JNumber yn]), Nothing) =>
                Just $ WallData (cast xn, cast yn)
              (Nothing, Just (JArray [JNumber x, JNumber y])) =>
                Just $ InvisibleWallData (x, y)
              _ => Nothing

getCreationData : Dict String JSON -> Checked CreationData
getCreationData dict = case extractWallData dict of
  Nothing => pure $ BoxData $ eitherToMaybe $ getVector "impulse" dict
  Just x => pure x

ObjectCaster Creation where
  objectCast dict = with Checked do
    let id = getId dict
    ref <- getString "ref" dict
    pos <- getVector "position" dict
    creationData <- getCreationData dict
    let angle = getDoubleOrDefault "angle" 0 dict
    pure $ MkCreation id ref pos angle (getTags' dict) creationData Nothing

public export
record Background where
  constructor MkBackground
  image : ResourceReference
  dimensions : Vector2D

ObjectCaster Background where
  objectCast dict = with Checked do
    ref <- getString "image" dict
    dimensions <- getVector "dimensions" dict
    pure $ MkBackground ref dimensions

public export
record MapDescriptor where
  constructor MkMapDescriptor
  name : String
  background : Background
  creations : List Creation
  dimensions : Vector2D

ObjectCaster MapDescriptor where
  objectCast dict = with Maybe do
    name <- getString "name" dict
    dimensions <- getVector "dimensions" dict
    background <- the (Checked Background) $ getCastable "background" dict
    creations' <- getArray "creations" dict
    let creations = fromMaybe [] $ eitherToMaybe $ catResults $ map cast creations'
    pure $ MkMapDescriptor name background creations dimensions

public export
GameIO m => SimpleLoader m MapDescriptor where
  load id = checkedJSONLoad (refToFilepath id) -- $ "res/main/maps/" ++ id ++ ".json"

public export
GameIO m => SimpleLoader m ObjectDescriptor where
  load id = checkedJSONLoad (refToFilepath id) -- $ "res/main/objects/" ++ id ++ ".json"

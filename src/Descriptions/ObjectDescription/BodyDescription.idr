module Descriptions.ObjectDescription.BodyDescription

import Data.Bits
import Physics.Box2D
import Physics.Vector2D

import GameIO
import Exception
import Descriptions.BitsDescription
import Descriptions.ObjectDescription.BodyFlags

export
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
  groupIndex : Maybe Int
  categoryBits : Maybe Int
  maskBits : Maybe Int
  name : Maybe String

export
Show FixtureParameters where
  show fp
    =  "{ offset: " ++ show (offset fp)
    ++ ", angle: " ++ show (angle fp)
    ++ ", density: " ++ show (density fp)
    ++ ", friction: " ++ show (friction fp)
    ++ ", restitution: " ++ show (restitution fp)
    ++ ", group index: " ++ show (groupIndex fp)
    ++ ", name: " ++ show (name fp)
    ++ " }"

export -- TODO rewrite with getMaybe or smth so it validates
getFixtureParameters : JSONDict -> Checked FixtureParameters
getFixtureParameters dict
  = let offset = eitherToMaybe $ getVector "offset" dict
        angle = eitherToMaybe $ getDouble "angle" dict
        density = eitherToMaybe $ getDouble "density" dict
        friction = eitherToMaybe $ getDouble "friction" dict
        restitution = eitherToMaybe $ getDouble "restitution" dict
        groupIndex = eitherToMaybe $ getInt "groupIndex" dict
        in with Checked do
          name <- getStringMaybe "name" dict
          categoryBits <- getBitsMaybe "categoryBits" dict
          maskBits <- getBitsMaybe "maskBits" dict
          pure $ MkFixtureParameters
            offset angle density friction restitution groupIndex categoryBits maskBits name

export
fixtureFromParametersShape : FixtureParameters -> Shape -> FixtureDefinition
fixtureFromParametersShape fp shape
  = MkFixtureDefinition shape (offset fp) (angle fp) (density fp) (friction fp)
          (restitution fp) (groupIndex fp) (categoryBits fp) (maskBits fp) (name fp)

export
ObjectCaster FixtureDefinition where
  objectCast dict = with Checked do
    fixture_parameters <- getFixtureParameters dict
    shape <- the (Checked Shape) $ getCastable "shape" dict
    pure $ fixtureFromParametersShape fixture_parameters shape

getFixtures : JSONDict -> Checked (List FixtureDefinition)
getFixtures dict = case hasKey "fixtures" dict of
  False => the (Checked FixtureDefinition) (getCastable "fixture" dict) >>= pure . pure
  True => getArray "fixtures" dict >>= catResults . map (the (Checked FixtureDefinition) . cast)

public export
data PhysicsEffect = Drag Double Vector2D

export
Show PhysicsEffect where
  show (Drag factor offset)
    = "{ factor: " ++ show factor ++ ", offset: " ++ show offset ++ " }"

export
ObjectCaster PhysicsEffect where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "drag" => with Checked do
        factor <- getDouble "factor" dict
        offset <- getVector "offset" dict
        pure $ Drag factor offset
      _ => fail "effect type must be of \"drag\""

getPhysicsEffects : JSONDict -> Checked (List PhysicsEffect)
getPhysicsEffects dict = case (hasKey "physicsEffect" dict,  hasKey "physicsEffects" dict) of
  (True, False) => with Checked do
    effect <- the (Checked PhysicsEffect) $ getCastable "physicsEffect" dict
    pure [effect]
  (False, True) => with Checked do
    effects' <- getArray "physicsEffects" dict
    pure $ the (List PhysicsEffect) $ catMaybes $ map (eitherToMaybe . cast) effects'
  (False, False) => pure empty
  (True, True) => fail "\"physicsEffect and physicsEffects can't both be present"

Cast String (Checked BodyType) where
  cast "static" = pure Static
  cast "dynamic" = pure Dynamic
  cast "kinematic" = pure Kinematic
  cast x = fail $
    "body type must be of \"static\"|\"dynamic\"|\"kinematic\", not \"" ++ x ++ "\""

public export
record BodyDescription where
  constructor MkBodyDescription
  type : BodyType
  fixedRotation : Maybe Bool
  bullet : Maybe Bool
  fixtures : List FixtureDefinition
  effects : List PhysicsEffect
  groupIndex : Maybe Int
  categoryBits : Maybe Int
  maskBits : Maybe Int
  flags : Maybe BodyFlags

export
Show BodyDescription where
  show bd
    =  "{ type: " ++ show (type bd)
    ++ ", fixedRotation: " ++ show (fixedRotation bd)
    ++ ", bullet: " ++ show (bullet bd)
    ++ ", fixtures: " ++ show (fixtures bd)
    ++ ", effects: " ++ show (effects bd)
    ++ ", group index: " ++ show (groupIndex bd)
    ++ ", category bits: " ++ show (categoryBits bd)
    ++ ", mask bits: " ++ show (maskBits bd)
    ++ " }"

export
ObjectCaster BodyDescription where
  objectCast dict = with Checked do
    typeString <- getString "type" dict
    type <- the (Checked BodyType) (cast typeString)
    let fixedRotation = eitherToMaybe $ getBool "fixedRotation" dict
    let bullet = eitherToMaybe $ getBool "bullet" dict
    fixtures <- getFixtures dict
    effects <- getPhysicsEffects dict
    let groupIndex = eitherToMaybe $ getInt "groupIndex" dict
    categoryBits <- getBitsMaybe "categoryBits" dict
    maskBits <- getBitsMaybe "maskBits" dict
    flags <- the (Checked (Maybe BodyFlags)) $ getCastableMaybe "flags" dict
    pure $ MkBodyDescription type fixedRotation bullet fixtures effects groupIndex
                             categoryBits maskBits flags

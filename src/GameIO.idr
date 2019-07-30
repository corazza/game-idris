module GameIO

import Control.ST
import public Language.JSON
import Graphics.SDL2 as SDL2
import public Data.AVL.Dict
import Physics.Vector2D
import Physics.Box2D
import Physics.Box2D.Definitions

import public Serializer
import Exception

public export
interface (Monad m, ConsoleIO m) => GameIO (m : Type -> Type) where
  ticks : STrans m Int xs (const xs)
  loadJSON : (filepath : String) -> m (Maybe JSON)
  checkedJSONLoad : (Cast JSON (Checked r), GameIO m) => (filepath : String) -> m (Checked r)
  write : (what : String) -> (filepath : String) -> m ()

  log : String -> m ()

  createWorld : Vector2D -> m Box2D.World
  destroyWorld : Box2D.World -> m ()
  createBody : Box2D.World -> BodyDefinition -> m (Int, Body)
  createFixture : Body -> FixtureDefinition -> m Fixture
  createFixture' : Body -> Shape -> Double -> m Fixture
  destroy : Box2D.World -> Body -> m ()
  step : Box2D.World ->
         (timeStep : Double) ->
         (velocityIterations : Int) ->
         (positionIterations : Int) ->
         m ()
  -- applyImpulse : Body -> Vector2D -> m ()
  applyForce : Body -> Vector2D -> Vector2D -> m ()
  getMass : Body -> m Double
  getPosition : Body -> m Vector2D
  getWorldCenter : Body -> m Vector2D
  getAngle : Body -> m Double
  getVelocity : Body -> m Vector2D
  -- pollEvent : Box2D.World -> m (Maybe Box2D.Event)
  pollEvents : Box2D.World -> m (List Box2D.Event)

public export
GameIO IO where
  ticks = lift getTicks

  loadJSON filepath = do
    Right mapJSON <- readFile filepath
                  | Left err => pure Nothing
    let parsed = parse mapJSON
    when (isNothing parsed) $
      putStrLn ("failed to parse: " ++ filepath ++ " (GameIO.loadJSON)")
    pure parsed

  checkedJSONLoad filepath = do
    Just a <- loadJSON filepath | pure (fail $ "couldn't load " ++ filepath)
    pure $ cast a

  write what filepath = do
    writeFile filepath what
    pure ()

  log = putStrLn

  createWorld = Box2D.createWorld
  destroyWorld = Box2D.destroyWorld
  createBody = Box2D.createBody
  createFixture = Box2D.createFixture
  createFixture' = Box2D.createFixture'
  destroy = Box2D.destroy
  step = Box2D.step
  -- applyImpulse = Box2D.applyImpulse
  applyForce = Box2D.applyForce
  getMass = Box2D.getMass
  getPosition = Box2D.getPosition
  getWorldCenter = Box2D.getWorldCenter
  getAngle = Box2D.getAngle
  getVelocity = Box2D.getVelocity
  -- pollEvent = Box2D.pollEvent
  pollEvents = Box2D.pollEvents


public export
ContentReference : Type
ContentReference = String
%name ContentReference ref

public export
interface ObjectCaster a where
  objectCast : JSONDict -> Checked a

export
ObjectCaster a => Cast JSON (Checked a) where
  cast (JObject xs) = objectCast (fromList xs)
  cast _ = fail "not a JSON object"

export
getCastable : ObjectCaster a => (name : String) -> (dict : JSONDict) -> Checked a
getCastable name dict = with Checked do
  aJSON <- maybeToEither ("JSON lookup fail for " ++ name) (lookup name dict)
  the (Checked a) (cast aJSON)

export
getCastableMaybe : ObjectCaster a => (name : String) -> (dict : JSONDict) -> Checked (Maybe a)
getCastableMaybe name dict = case hasKey name dict of
  False => pure Nothing
  True => getCastable name dict >>= pure . Just

export
getCastableOrDefault : ObjectCaster a => a -> (name : String) -> (dict : JSONDict) -> a
getCastableOrDefault x name dict = let castable = eitherToMaybe (getCastable name dict) in
  fromMaybe x castable

total
keyError : (type : String) -> (key : String) -> String
keyError type key = type ++ " \"" ++ key ++ "\" inexistent"

export total
getVector : (name : String) -> (dict : JSONDict) -> Checked Vector2D
getVector name dict = with Checked do
  JArray [JNumber x, JNumber y] <- maybeToEither (keyError "vector" name) (lookup name dict)
                                | fail ("vector format fail" ++ name)
  pure (x, y)

export total
getInt : (name : String) -> (dict : JSONDict) -> Checked Int
getInt name dict = with Checked do
  JNumber x <- maybeToEither (keyError "int" name) (lookup name dict)
            | fail ("int format fail " ++ name)
  pure (cast x)

export total
getIntPair : (name : String) -> (dict : JSONDict) -> Checked (Int, Int)
getIntPair name dict = with Checked do
  JArray [JNumber x, JNumber y] <- maybeToEither (keyError "int pair" name) (lookup name dict)
                                | fail ("int pair format fail " ++ name)
  pure (cast x, cast y)

export total
getNatPair : (name : String) -> (dict : JSONDict) -> Checked (Nat, Nat)
getNatPair name dict = with Checked do
  JArray [JNumber x, JNumber y] <- maybeToEither (keyError "nat pair" name) (lookup name dict)
                                | fail ("nat pair format fail " ++ name)
  pure (toNat $ the Int $ cast x, toNat $ the Int $ cast y)


export
getDouble : String -> JSONDict -> Checked Double
getDouble key dict = case lookup key dict of
  Just (JNumber x) => pure x
  _ => fail $ "not a double (" ++ key ++ ")"

export
getDoubleMaybe : String -> JSONDict -> Checked (Maybe Double)
getDoubleMaybe key dict = case hasKey key dict of
  False => pure Nothing
  True => getDouble key dict >>= pure . Just

export
getBool : String -> JSONDict -> Checked Bool
getBool key dict = case lookup key dict of
  Just (JBoolean x) => pure x
  _ => fail $ "not a boolean (" ++ key ++ ")"

export
getBoolOrDefault : Bool -> String -> JSONDict -> Checked Bool
getBoolOrDefault default key dict = case lookup key dict of
  Nothing => pure default
  Just (JBoolean x) => pure x
  _ => fail $ "not a boolean (" ++ key ++ ")"

export
getBoolMaybe : String -> JSONDict -> Checked (Maybe Bool)
getBoolMaybe key dict = case hasKey key dict of
  False => pure Nothing
  True => getBool key dict >>= pure . Just

export
getString : String -> JSONDict -> Checked String
getString key dict = case lookup key dict of
  Just (JString x) => pure x
  _ => fail $ "not a string (" ++ key ++ ")"

export
getStringMaybe : String -> JSONDict -> Checked (Maybe String)
getStringMaybe key dict = case hasKey key dict of
  False => pure Nothing
  True => getString key dict >>= pure . Just

export
getArray : String -> JSONDict -> Checked (List JSON)
getArray key dict = case lookup key dict of
  Just (JArray xs) => pure xs
  _ => fail $ "not an array (" ++ key ++ ")"

export
getStrings : String -> JSONDict -> Checked (List String)
getStrings key dict = (case lookup key dict of
  Just (JArray xs) => getStrings' [] xs
  Nothing => fail $ "not an array (" ++ key ++ ")") where
      getStrings' : List String -> List JSON -> Checked (List String)
      getStrings' acc [] = pure acc
      getStrings' acc (JString x :: xs) = getStrings' (x :: acc) xs
      getStrings' acc (_ :: xs) = fail $ "not a string (in array " ++ key ++ ")"

-- TODO fix, should typecheck
export
getDoubleOrDefault : String -> Double -> JSONDict -> Double
getDoubleOrDefault key default dict = case lookup key dict of
  Just (JNumber x) => x
  _ => default

export
getIntOrDefault : String -> Int -> JSONDict -> Int
getIntOrDefault key default dict = case lookup key dict of
  Just (JNumber x) => cast x
  _ => default

export
maybeFromString : (name : String) ->
                  (conv : String -> Checked a) ->
                  (dict : JSONDict) ->
                  Checked (Maybe a)
maybeFromString name conv dict = case lookup name dict of
  Nothing => pure Nothing
  Just (JString x) => case conv x of
    Left e => fail e
    Right r => pure $ Just r
  _ => fail $ name ++ " must be string"

export
jsonToString : JSON -> Checked String
jsonToString (JString x) = pure x
jsonToString _ = fail "not a string"

export
refToFilepath : ContentReference -> String
refToFilepath = (++) "res/"

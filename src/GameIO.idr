module GameIO

import Control.ST
import Language.JSON
import Graphics.SDL2 as SDL2
import Data.AVL.Dict
import Common
import Physics.Vector2D
import Physics.Box2D
import Physics.Box2D.Definitions
import Exception
-- import Objects

public export
interface (Monad m, ConsoleIO m) => GameIO (m : Type -> Type) where
  EmptyContext : Type
  createEmptyContext : ST m Var [add EmptyContext]
  deleteEmptyContext : (c : Var) -> ST m () [remove c EmptyContext]

  ticks : STrans m Int xs (const xs)
  loadJSON : (filepath : String) -> m (Maybe JSON)

  log : Show a => a -> m ()

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
  applyImpulse : Body -> Vector2D -> m ()
  getMass : Body -> m Double
  getPosition : Body -> m Vector2D
  getAngle : Body -> m Double
  getVelocity : Body -> m Vector2D
  pollEvent : Box2D.World -> m (Maybe Box2D.Event)
  pollEvents : Box2D.World -> m (List Box2D.Event)

public export
GameIO IO where
  EmptyContext = State () -- TODO ugly get rid of it
  createEmptyContext = new ()
  deleteEmptyContext c = delete c

  ticks = lift getTicks
  loadJSON filepath = do
    Right mapJSON <- readFile filepath
                  | Left err => pure Nothing
    let parsed = parse mapJSON
    when (isNothing parsed) $
      putStrLn ("failed to parse: " ++ filepath ++ " (GameIO.loadJSON)")
    pure parsed

  log a = putStrLn $ show a

  createWorld = Box2D.createWorld
  destroyWorld = Box2D.destroyWorld
  createBody = Box2D.createBody
  createFixture = Box2D.createFixture
  createFixture' = Box2D.createFixture'
  destroy = Box2D.destroy
  step = Box2D.step
  applyImpulse = Box2D.applyImpulse
  getMass = Box2D.getMass
  getPosition = Box2D.getPosition
  getAngle = Box2D.getAngle
  getVelocity = Box2D.getVelocity
  pollEvent = Box2D.pollEvent
  pollEvents = Box2D.pollEvents


export
interface ObjectCaster a where
  objectCast : Dict String JSON -> Checked a

export
ObjectCaster a => Cast JSON (Checked a) where
  cast (JObject xs) = objectCast (fromList xs)
  cast _ = fail "not a JSON object"

export
getCastable : ObjectCaster a => (name : String) -> (dict : Dict String JSON) -> Checked a
getCastable name dict = with Checked do
  aJSON <- maybeToEither ("JSON lookup fail for " ++ name) (lookup name dict)
  the (Checked a) (cast aJSON)

export
getCastableOrDefault : ObjectCaster a => a -> (name : String) -> (dict : Dict String JSON) -> a
getCastableOrDefault x name dict = let castable = eitherToMaybe (getCastable name dict) in
  fromMaybe x castable

export
getColor : (name : String) -> (dict : Dict String JSON) -> Checked Color
getColor name dict = with Checked do
  JArray [JNumber r, JNumber g, JNumber b, JNumber a] <-
    maybeToEither "color format fail" (lookup name dict)
  pure $ MkColor (cast r) (cast g) (cast b) (cast a)

export
getVector : (name : String) -> (dict : Dict String JSON) -> Checked Vector2D
getVector name dict = with Checked do
  JArray [JNumber x, JNumber y] <- maybeToEither "vector format fail" (lookup name dict)
  pure (x, y)

export
getDouble : String -> Dict String JSON -> Checked Double
getDouble key dict = case lookup key dict of
  Just (JNumber x) => pure x
  _ => fail $ "not a double (" ++ key ++ ")"

export
getBool : String -> Dict String JSON -> Checked Bool
getBool key dict = case lookup key dict of
  Just (JBoolean x) => pure x
  _ => fail $ "not a boolean (" ++ key ++ ")"

export
getString : String -> Dict String JSON -> Checked String
getString key dict = case lookup key dict of
  Just (JString x) => pure x
  _ => fail $ "not a string (" ++ key ++ ")"

export
getArray : String -> Dict String JSON -> Checked (List JSON)
getArray key dict = case lookup key dict of
  Just (JArray xs) => pure xs
  _ => fail $ "not an array (" ++ key ++ ")"

export
getStrings : String -> Dict String JSON -> Checked (List String)
getStrings key dict = (case lookup key dict of
  Just (JArray xs) => getStrings' [] xs
  Nothing => fail $ "not an array (" ++ key ++ ")") where
      getStrings' : List String -> List JSON -> Checked (List String)
      getStrings' acc [] = pure acc
      getStrings' acc (JString x :: xs) = getStrings' (x :: acc) xs
      getStrings' acc (_ :: xs) = fail $ "not a string (in array " ++ key ++ ")"

export
getDoubleOrDefault : String -> Double -> Dict String JSON -> Double
getDoubleOrDefault key default dict = case lookup key dict of
  Just (JNumber x) => x
  _ => default

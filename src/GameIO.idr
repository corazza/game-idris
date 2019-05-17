module GameIO

import Control.ST
import Language.JSON
import Graphics.SDL2 as SDL2
import Data.AVL.Dict
import Common

import Physics.Vector2D

public export
interface GameIO (m : Type -> Type) where
  EmptyContext : Type
  createEmptyContext : ST m Var [add EmptyContext]
  deleteEmptyContext : (c : Var) -> ST m () [remove c EmptyContext]

  ticks : STrans m Int xs (const xs)
  loadJSON : (filepath : String) -> m (Maybe JSON)

public export
GameIO IO where
  EmptyContext = State ()
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

export
interface ObjectCaster a where
  objectCast : Dict String JSON -> Maybe a

export
ObjectCaster a => Cast JSON (Maybe a) where
  cast (JObject xs) = objectCast (fromList xs)
  cast _ = Nothing

export
getColor : (name : String) -> (dict : Dict String JSON) -> Maybe Color
getColor name dict = with Maybe do
  JArray [JNumber r, JNumber g, JNumber b, JNumber a] <- lookup name dict | Nothing
  pure $ MkColor (cast r) (cast g) (cast b) (cast a)

export
getVector : (name : String) -> (dict : Dict String JSON) -> Maybe Vector2D
getVector name dict = with Maybe do
  JArray [JNumber x, JNumber y] <- lookup name dict | Nothing
  pure (x, y)

export
getDouble : String -> Dict String JSON -> Maybe Double
getDouble key dict = case lookup key dict of
  Just (JNumber x) => Just x
  _ => Nothing

export
getDoubleOrDefault : String -> Double -> Dict String JSON -> Double
getDoubleOrDefault key default dict = case lookup key dict of
  Just (JNumber x) => x
  _ => default

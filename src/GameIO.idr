module GameIO

import Control.ST
import Language.JSON
import Graphics.SDL2 as SDL2



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

module SDL.Color

import GameIO
import Exception

public export
data Color = MkColor Int Int Int Int

export
getColor : (name : String) -> (dict : JSONDict) -> Checked Color
getColor name dict = with Checked do
  JArray [JNumber r, JNumber g, JNumber b, JNumber a] <-
    maybeToEither ("color \"" ++ name ++ "\" inexistent") (lookup name dict)
                  | fail "color format fail (must be [r, g, b])"
  pure $ MkColor (cast r) (cast g) (cast b) (cast a)

module Descriptions.SurfaceDescription

import GameIO
import Exception
import Descriptions.Color
import Objects

public export
data SurfaceRenderMethod
  = Image ContentReference
  | Colored Color

ObjectCaster SurfaceRenderMethod where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "color" => getColor "color" dict >>= pure . Colored
      "image" => getString "image" dict >>= pure . Image
      x => fail $ x ++ " is not a valid surface render type"

public export
record SurfaceRenderDescription where
  constructor MkSurfaceRenderDescription
  inactive : SurfaceRenderMethod
  hover : Maybe SurfaceRenderMethod
  clicked : Maybe SurfaceRenderMethod

ObjectCaster SurfaceRenderDescription where
  objectCast dict = case hasKey "inactive" dict of
    True => with Checked do
      inactive <- the (Checked SurfaceRenderMethod) $ getCastable "inactive" dict
      hover <- the (Checked (Maybe SurfaceRenderMethod)) $ getCastableMaybe "hover" dict
      clicked <- the (Checked (Maybe SurfaceRenderMethod)) $ getCastableMaybe "clicked" dict
      pure $ MkSurfaceRenderDescription inactive hover clicked
    False => with Checked do
      inactive <- the (Checked SurfaceRenderMethod) $ objectCast dict
      pure $ MkSurfaceRenderDescription inactive Nothing Nothing

public export
data DisplayStyle = Center
                  | Up | Down | Left | Right
                  | UpLeft | UpRight | DownLeft | DownRight

stringDisplayStyleDict : Dict String DisplayStyle
stringDisplayStyleDict = fromList [
  ("center", Center),
  ("up", Up),
  ("down", Down),
  ("left", Left),
  ("right", Right),
  ("upLeft", UpLeft),
  ("upRight", UpRight),
  ("downLeft", DownLeft),
  ("downRight", DownRight)
]

Cast String (Checked DisplayStyle) where
  cast x = pick "displayStyle" x stringDisplayStyleDict

public export
data Layout = Vertical | Horizontal

stringLayoutDict : Dict String Layout
stringLayoutDict = fromList [
  ("vertical", Vertical),
  ("horizontal", Horizontal)
]

Cast String (Checked Layout) where
  cast x = pick "layout" x stringLayoutDict

public export
record SurfaceParameters where
  constructor MkSurfaceParameters
  dimensions : Maybe (Int, Int)
  render : SurfaceRenderDescription
  click : Maybe String
  displayStyle : DisplayStyle
  layout : Layout

getDimensions : JSONDict -> Checked (Maybe (Int, Int))
getDimensions dict = case hasKey "width" dict of
  True => with Checked do
    width <- getInt "width" dict
    height <- getInt "height" dict
    pure $ Just (width, height)
  False => pure Nothing

export
ObjectCaster SurfaceParameters where
  objectCast dict = with Checked do
    dimensions <- getDimensions dict
    render <- the (Checked SurfaceRenderDescription) $ getCastable "render" dict
    click <- getStringMaybe "click" dict
    displayStyle' <- getStringOrDefault "displayStyle" "center" dict
    displayStyle <- cast displayStyle'
    layout' <- getStringOrDefault "layout" "vertical" dict
    layout <- cast layout'
    pure $ MkSurfaceParameters dimensions render click displayStyle layout

public export
record SurfaceDescription where
  constructor MkSurfaceDescription
  id : Maybe SurfaceId
  surfaceParameters : SurfaceParameters
  children : List SurfaceDescription

mutual
  getChildren : JSONDict -> Checked (List SurfaceDescription)
  getChildren dict = case hasKey "children" dict of
    True => getArray "children" dict >>= catResults . map cast
    False => pure []

  export
  ObjectCaster SurfaceDescription where
    objectCast dict = with Checked do
      id <- getStringMaybe "id" dict
      surfaceParameters <- the (Checked SurfaceParameters) $ objectCast dict
      children <- getChildren dict
      pure $ MkSurfaceDescription id surfaceParameters children

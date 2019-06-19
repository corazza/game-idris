module Settings

import Data.AVL.Dict
import Language.JSON

import Physics.Vector2D
import Common
import GameIO

export
defaultHealthColor : Color
defaultHealthColor = MkColor 10 223 76 (cast (255*0.71))

public export
record Settings where
  constructor MkSettings
  fullHealthWidth : Int
  fullHealthHeight : Int
  healthYD : Int
  healthColor : Color
  resolution : (Int, Int)
%name Settings settings

export
defaultSettings : Settings
defaultSettings = MkSettings 60 7 30 defaultHealthColor (1280, 800)

ObjectCaster Settings where
  objectCast dict = with Maybe do
    JNumber fullHealthWidth <- lookup "fullHealthWidth" dict
    JNumber fullHealthHeight <- lookup "fullHealthHeight" dict
    JNumber healthYD <- lookup "healthYD" dict
    color <- getColor "healthColor" dict
    resolution <- getVector "resolution" dict
    pure $ MkSettings (cast fullHealthWidth) (cast fullHealthHeight)
                      (cast healthYD) color (cast resolution)

export
loadSettings : (Monad m, GameIO m) => (path : String) -> m (Maybe Settings)
loadSettings path = do Just a <- loadJSON path | pure Nothing
                       pure (cast a)

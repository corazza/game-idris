module Settings

import Data.AVL.Dict
import Language.JSON

import Physics.Vector2D
import Common
import GameIO
import Exception

export
defaultHealthColor : Color
defaultHealthColor = MkColor 10 223 76 (cast (255*0.71))

public export
record SceneSettings where
  constructor MkSceneSettings
  gravity : Double
%name SceneSettings sceneSettings

ObjectCaster SceneSettings where
  objectCast dict = with Checked do
    gravity <- getDouble "gravity" dict
    pure $ MkSceneSettings (-gravity)

export
defaultSceneSettings : SceneSettings
defaultSceneSettings = MkSceneSettings (-8.0)

public export
record Settings where
  constructor MkSettings
  fullHealthWidth : Int
  fullHealthHeight : Int
  healthYD : Int
  healthColor : Color
  resolution : (Int, Int)
  sceneSettings : SceneSettings
%name Settings settings

export
defaultSettings : Settings
defaultSettings = MkSettings 60 7 30 defaultHealthColor (1280, 800) defaultSceneSettings

ObjectCaster Settings where
  objectCast dict = with Maybe do
    fullHealthWidth <- getDouble "fullHealthWidth" dict
    fullHealthHeight <- getDouble "fullHealthHeight" dict
    healthYD <- getDouble "healthYD" dict
    color <- getColor "healthColor" dict
    resolution <- getVector "resolution" dict
    let sceneSettings = getCastableOrDefault defaultSceneSettings "scene" dict
    pure $ MkSettings (cast fullHealthWidth) (cast fullHealthHeight)
                      (cast healthYD) color (cast resolution) sceneSettings

export
loadSettings : (Monad m, GameIO m) => (path : String) -> m (Checked Settings)
loadSettings path = do Just a <- loadJSON path | pure (fail $ "couldn't load: " ++ path)
                       pure (cast a)
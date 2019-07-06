module Settings

import Data.AVL.Dict
import Language.JSON

import Physics.Vector2D
import Common
import GameIO
import Exception

public export
record UISettings where
  constructor MkUISettings
  fullHealthWidth : Int
  fullHealthHeight : Int
  healthYD : Int
  healthColor : Color

ObjectCaster UISettings where
  objectCast dict = with Checked do
    fullHealthWidth <- getInt "fullHealthWidth" dict
    fullHealthHeight <- getInt "fullHealthHeight" dict
    healthYD <- getInt "healthYD" dict
    color <- getColor "healthColor" dict
    pure $ MkUISettings fullHealthWidth fullHealthHeight healthYD color

export
defaultUISettings : UISettings
defaultUISettings = MkUISettings 60 7 30 (MkColor 10 223 76 181)


public export
record DisplaySettings where
  constructor MkDisplaySettings
  resolution : (Int, Int)
  cameraYD : Double
  zoom : Double

ObjectCaster DisplaySettings where
  objectCast dict = with Checked do
    resolution <- getVector "resolution" dict
    cameraYD <- getDouble "cameraYD" dict
    zoom <- getDouble "zoom" dict
    pure $ MkDisplaySettings (cast resolution) cameraYD zoom

export
defaultDisplaySettings : DisplaySettings
defaultDisplaySettings = MkDisplaySettings (1280, 800) 50 45.0


public export
record SceneSettings where
  constructor MkSceneSettings
  gravity : Double
  timeStep : Int
%name SceneSettings sceneSettings

ObjectCaster SceneSettings where
  objectCast dict = with Checked do
    gravity <- getDouble "gravity" dict
    timeStep <- getInt "timeStep" dict
    pure $ MkSceneSettings (-gravity) timeStep

export
defaultSceneSettings : SceneSettings
defaultSceneSettings = MkSceneSettings (-9.0) 15


public export
record Settings where
  constructor MkSettings
  uiSettings : UISettings
  displaySettings : DisplaySettings
  sceneSettings : SceneSettings
%name Settings settings

export
defaultSettings : Settings
defaultSettings = MkSettings defaultUISettings defaultDisplaySettings defaultSceneSettings

ObjectCaster Settings where
  objectCast dict = with Maybe do
    let uiSettings = getCastableOrDefault defaultUISettings "ui" dict
    let displaySettings = getCastableOrDefault defaultDisplaySettings "display" dict
    let sceneSettings = getCastableOrDefault defaultSceneSettings "scene" dict
    pure $ MkSettings uiSettings displaySettings sceneSettings

export
loadSettings : (Monad m, GameIO m) => (path : String) -> m (Checked Settings)
loadSettings path = do Just a <- loadJSON path | pure (fail $ "couldn't load: " ++ path)
                       pure (cast a)

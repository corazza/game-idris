module Settings

import Physics.Vector2D

import GameIO
import Exception
import Descriptions.Color

public export
record CameraSettings where
  constructor MkCameraSettings
  resolution : (Int, Int)
  yd : Double
  zoom : Double

ObjectCaster CameraSettings where
  objectCast dict = with Checked do
    resolution <- getIntPair "resolution" dict
    yd <- getDouble "yd" dict
    zoom <- getDouble "zoom" dict
    pure $ MkCameraSettings resolution yd zoom

public export
defaultCameraSettings : CameraSettings
defaultCameraSettings = MkCameraSettings (1280, 800) 50 45.0

public export
record InfoSettings where
  constructor MkInfoSettings
  fullHealthWidth : Int
  fullHealthHeight : Int
  healthYD : Int
  healthColor : Color

ObjectCaster InfoSettings where
  objectCast dict = with Checked do
    fullHealthWidth <- getInt "fullHealthWidth" dict
    fullHealthHeight <- getInt "fullHealthHeight" dict
    healthYD <- getInt "healthYD" dict
    color <- getColor "healthColor" dict
    pure $ MkInfoSettings fullHealthWidth fullHealthHeight healthYD color

public export
defaultInfoSettings : InfoSettings
defaultInfoSettings = MkInfoSettings 60 7 30 (MkColor 10 223 76 181)


public export
record RenderingSettings where
  constructor MkRenderingSettings
  cameraSettings : CameraSettings
  infoSettings : InfoSettings

ObjectCaster RenderingSettings where
  objectCast dict = with Checked do
    cameraSettings <- the (Checked CameraSettings) $ getCastable "camera" dict
    infoSettings <- the (Checked InfoSettings) $ getCastable "info" dict
    pure $ MkRenderingSettings cameraSettings infoSettings

public export
defaultRenderingSettings : RenderingSettings
defaultRenderingSettings
  = MkRenderingSettings defaultCameraSettings defaultInfoSettings

public export
record ClientSettings where
  constructor MkClientSettings
  renderingSettings : RenderingSettings

export
ObjectCaster ClientSettings where
  objectCast dict = with Checked do
    renderingSettings <- the (Checked RenderingSettings) $ getCastable "rendering" dict
    pure $ MkClientSettings renderingSettings

export
resolutionX : ClientSettings -> Int
resolutionX = fst . resolution . cameraSettings . renderingSettings

export
resolutionY : ClientSettings -> Int
resolutionY = snd . resolution . cameraSettings . renderingSettings

public export
defaultClientSettings : ClientSettings
defaultClientSettings = MkClientSettings defaultRenderingSettings

public export
record DynamicsSettings where
  constructor MkDynamicsSettings
  gravity : Vector2D
  timeStep : Int

export
ObjectCaster DynamicsSettings where
  objectCast dict = with Checked do
    gravity <- getVector "gravity" dict
    timeStep <- getInt "timeStep" dict
    pure $ MkDynamicsSettings gravity timeStep

public export
defaultDynamicsSettings : DynamicsSettings
defaultDynamicsSettings = MkDynamicsSettings (0, -9.81) 15

public export
record ServerSettings where
  constructor MkServerSettings
  dummy : Int

export
ObjectCaster ServerSettings where
  objectCast dict = pure $ MkServerSettings 0

public export
defaultServerSettings : ServerSettings
defaultServerSettings = MkServerSettings 0

-- TODO LATER make simpleLoad interface where someMethod = cast

export
loadDynamicsSettings : GameIO m => m (Checked DynamicsSettings)
loadDynamicsSettings = with m do
  Just json <- loadJSON "settings/dynamics.json"
          | Nothing => pure (fail "couldn't load dynamics settings")
  pure $ cast json

export
loadClientSettings : GameIO m => m (Checked ClientSettings)
loadClientSettings = with m do
  Just json <- loadJSON "settings/client.json"
          | Nothing => pure (fail "couldn't load client settings")
  pure $ cast json

export
loadServerSettings : GameIO m => m (Checked ServerSettings)
loadServerSettings = with m do
  Just json <- loadJSON "settings/server.json"
          | Nothing => pure (fail "couldn't load server settings")
  pure $ cast json

public export
record GameSettings where
  constructor MkGameSettings
  dynamics : DynamicsSettings
  server : ServerSettings
  client : ClientSettings

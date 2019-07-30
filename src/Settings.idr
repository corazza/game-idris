module Settings

import Physics.Vector2D
import Control.ST
import Control.ST.ImplicitCall
import Control.Monad.Identity
import public Language.JSON

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

Serialize CameraSettings where
  toDict cameraSettings = with ST do
    cameraObject <- makeObject
    addIntPair cameraObject "resolution" (resolution cameraSettings)
    addDouble cameraObject "yd" (yd cameraSettings)
    addDouble cameraObject "zoom" (zoom cameraSettings)
    getDict cameraObject

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

Serialize InfoSettings where
  toDict infoSettings = with ST do
    infoObject <- makeObject
    addInt infoObject "fullHealthWidth" (fullHealthWidth infoSettings)
    addInt infoObject "fullHealthHeight" (fullHealthHeight infoSettings)
    addInt infoObject "healthYD" (healthYD infoSettings)
    addInt infoObject "fullHealthWidth" (fullHealthWidth infoSettings)
    addColor infoObject "healthColor" (healthColor infoSettings)
    getDict infoObject

public export
defaultInfoSettings : InfoSettings
defaultInfoSettings = MkInfoSettings 60 7 30 (MkColor 10 223 76 181)

public export
record RenderingSettings where
  constructor MkRenderingSettings
  cameraSettings : CameraSettings
  infoSettings : InfoSettings

export
setCameraSettings : CameraSettings -> RenderingSettings -> RenderingSettings
setCameraSettings cameraSettings' = record { cameraSettings = cameraSettings' }

export
setInfoSettings : InfoSettings -> RenderingSettings -> RenderingSettings
setInfoSettings infoSettings' = record { infoSettings = infoSettings' }

ObjectCaster RenderingSettings where
  objectCast dict = with Checked do
    cameraSettings <- the (Checked CameraSettings) $ getCastable "camera" dict
    infoSettings <- the (Checked InfoSettings) $ getCastable "info" dict
    pure $ MkRenderingSettings cameraSettings infoSettings

Serialize RenderingSettings where
  toDict renderingSettings = with ST do
    cameraObject <- toDict $ cameraSettings renderingSettings
    infoObject <- toDict $ infoSettings renderingSettings
    renderingObject <- makeObject
    addObject renderingObject "camera" cameraObject
    addObject renderingObject "info" infoObject
    getDict renderingObject

public export
defaultRenderingSettings : RenderingSettings
defaultRenderingSettings
  = MkRenderingSettings defaultCameraSettings defaultInfoSettings

public export
record ClientSettings where
  constructor MkClientSettings
  renderingSettings : RenderingSettings

export
setRenderingSettings : RenderingSettings -> ClientSettings -> ClientSettings
setRenderingSettings rs = record { renderingSettings = rs }

export
ObjectCaster ClientSettings where
  objectCast dict = with Checked do
    renderingSettings <- the (Checked RenderingSettings) $ getCastable "rendering" dict
    pure $ MkClientSettings renderingSettings

export
Serialize ClientSettings where
  toDict clientSettings = with ST do
    renderingObject <- toDict $ renderingSettings clientSettings
    clientObject <- makeObject
    addObject clientObject "rendering" renderingObject
    getDict clientObject

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

export
Serialize DynamicsSettings where
  toDict dynamicsSettings = with ST do
    dynamicsObject <- makeObject
    addVector dynamicsObject "gravity" $ gravity dynamicsSettings
    addInt dynamicsObject "timeStep" $ timeStep dynamicsSettings
    getDict dynamicsObject

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

export
Serialize ServerSettings where
  toDict serverSettings = with ST do
    serverObject <- makeObject
    addInt serverObject "dummy" $ dummy serverSettings
    getDict serverObject

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

export
saveDynamicsSettings : GameIO m => DynamicsSettings -> m ()
saveDynamicsSettings dynamicsSettings
  = write (pretty dynamicsSettings) "settings/dynamics.json"

export
saveClientSettings : GameIO m => ClientSettings -> m ()
saveClientSettings clientSettings
  = write (pretty clientSettings) "settings/client.json"

export
saveServerSettings : GameIO m => ServerSettings -> m ()
saveServerSettings serverSettings
  = write (pretty serverSettings) "settings/server.json"

public export
record GameSettings where
  constructor MkGameSettings
  dynamics : DynamicsSettings
  server : ServerSettings
  client : ClientSettings

export
saveSettings : GameIO m => GameSettings -> m ()
saveSettings gs = with m do
  saveDynamicsSettings $ dynamics gs
  saveClientSettings $ client gs
  saveServerSettings $ server gs

export
setClientSettings : ClientSettings -> GameSettings -> GameSettings
setClientSettings cs = record { client = cs }

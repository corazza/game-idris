module Settings

import Physics.Vector2D

public export
record CameraSettings where
  constructor MkCameraSettings
  resolution : (Int, Int)
  yd : Double
  zoom : Double

public export
defaultCameraSettings : CameraSettings
defaultCameraSettings = MkCameraSettings (1280, 800) 50 45.0

public export
record RenderingSettings where
  constructor MkRenderingSettings
  cameraSettings : CameraSettings

public export
defaultRenderingSettings : RenderingSettings
defaultRenderingSettings = MkRenderingSettings defaultCameraSettings

public export
record ClientSettings where
  constructor MkClientSettings
  renderingSettings : RenderingSettings

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

public export
defaultDynamicsSettings : DynamicsSettings
defaultDynamicsSettings = MkDynamicsSettings (0, -9.81)

public export
record ServerSettings where
  constructor MkServerSettings
  dummy : Int

public export
defaultServerSettings : ServerSettings
defaultServerSettings = MkServerSettings 0

public export
record Settings where
  constructor MkSettings
  dynamicsSettings : DynamicsSettings
  clientSettings : ClientSettings
  serverSettings : ServerSettings

public export
defaultSettings : Settings
defaultSettings = MkSettings
  defaultDynamicsSettings defaultClientSettings defaultServerSettings

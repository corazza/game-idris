module Editor

import Graphics.SDL2

import Client.UI
import Client.Rendering
import Client.Rendering.Camera
import Client.Input
import Client.SDL
import Client.ClientCommands
import Descriptions.MapDescription
import Descriptions.SurfaceDescription
import GameIO
import Objects
import JSONCache
import Settings
import Commands
import Editor.PEditor

public export
interface Editor (m : Type -> Type) where

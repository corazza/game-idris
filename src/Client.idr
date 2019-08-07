module Client

import Control.ST
import Control.ST.ImplicitCall

import Client.PClient
import Client.UI
import Client.Rendering
import Client.Rendering.PRendering
import Client.Rendering.Camera
import Client.SDL
import Client.Input
import Client.ClientCommands
import Server.PServer
import Descriptions.MapDescription
import Descriptions.ObjectDescription
import Descriptions.ObjectDescription.RenderDescription
import Dynamics
import Dynamics.PDynamics
import JSONCache
import GameIO
import Settings
import Exception
import Objects
import Commands
import Timeline

public export
data ClientState = Disconnected | Connected

public export
interface Client (m : Type -> Type) where
  SClient : ClientState -> Type

  startClient : (settings : ClientSettings) ->
                (preload : PreloadResults) ->
                ST m Var [add (SClient Disconnected)]
  endClient : (client : Var) -> ST m () [remove client (SClient Disconnected)]

  connect : (client : Var) ->
            (map : ContentReference) ->
            (characterId : ObjectId) ->
            ST m (Checked ()) [client ::: SClient Disconnected :->
                                \res => if isRight res
                                            then (SClient Connected)
                                            else (SClient Disconnected)]

  disconnect : (client : Var) ->
               ST m () [client ::: SClient Connected :-> SClient Disconnected]

  queryPClient : (client : Var) -> (q : PClient -> a) -> ST m a [client ::: SClient s]
  updatePClient : (client : Var) -> (f : PClient -> PClient) -> ST m () [client ::: SClient s]
  querySessionData : (client : Var) -> (q : SessionData -> a) -> ST m a [client ::: SClient Connected]

  -- processes server commands and strips own controls NETWORKING
  runServerCommand : (client : Var) ->
                     InSession ->
                     ST m () [client ::: SClient Connected]
  runServerCommands : (client : Var) ->
                      List InSession ->
                      ST m () [client ::: SClient Connected]

  -- gets input, converts to own commands, processes them, returns for sending to server
  iterate : (client : Var) ->
            (bodyData : Objects BodyData) ->
            ST m (Either () (List Command)) [client ::: SClient Connected]

  getSettings : (client : Var) -> ST m ClientSettings [client ::: SClient Connected]

  private
  updatePRendering : (client : Var) ->
                     (f : PRendering -> PRendering) ->
                     ST m () [client ::: SClient Connected]

  private
  runCommand : (client : Var) -> (command : Command) -> ST m () [client ::: SClient Connected]
  private
  runCommands : (client : Var) -> (commands : List Command) -> ST m () [client ::: SClient Connected]

  private
  runClientCommand : (client : Var) ->
                     (clientCommand : ClientCommand) ->
                     ST m () [client ::: SClient Connected]
  private
  runClientCommands : (client : Var) ->
                      (clientCommands : List ClientCommand) ->
                      ST m () [client ::: SClient Connected]

  private
  addObject : (client : Var) ->
              (id : ObjectId) ->
              (ref : ContentReference) ->
              ST m () [client ::: SClient Connected]
  private
  removeObject : (client : Var) ->
                 (id : ObjectId) ->
                 ST m () [client ::: SClient Connected]

  private
  refreshSettings : (client : Var) -> ST m () [client ::: SClient Connected]

export
(GameIO m, Rendering m, SDL m) => Client m where
  SClient Disconnected = Composite [State PClient, SUI {m}, SSDL {m}]
  SClient Connected = Composite [State PClient,
                                 State SessionData,
                                 SRendering {m},
                                 SUI {m},
                                 SSDL {m}]

  startClient settings preload = with ST do
    pclient <- new $ MkPClient preload settings
    sdl <- startSDL (resolutionX settings) (resolutionY settings)
    ui <- startUI preload
    client <- new ()
    combine client [pclient, ui, sdl]
    pure client

  endClient client = with ST do
    [pclient, ui, sdl] <- split client
    endSDL sdl
    endUI ui
    delete pclient
    delete client

  queryPClient client q {s} = case s of
    Disconnected => with ST do
      [pclient, ui, sdl] <- split client
      pclient' <- read pclient
      combine client [pclient, ui, sdl]
      pure $ q pclient'
    Connected => with ST do
      [pclient, session_data, rendering, ui, sdl] <- split client
      pclient' <- read pclient
      combine client [pclient, session_data, rendering, ui, sdl]
      pure $ q pclient'

  updatePClient client f {s} = case s of
    Disconnected => with ST do
      [pclient, ui, sdl] <- split client
      update pclient f
      combine client [pclient, ui, sdl]
    Connected => with ST do
      [pclient, session_data, rendering, ui, sdl] <- split client
      update pclient f
      combine client [pclient, session_data, rendering, ui, sdl]

  querySessionData client q = with ST do
      [pclient, session_data, rendering, ui, sdl] <- split client
      session_data' <- read session_data
      combine client [pclient, session_data, rendering, ui, sdl]
      pure $ q session_data'

  connect client map_ref characterId = with ST do
    preload <- queryPClient client preload {s=Disconnected}
    case getMapDescription map_ref preload of
      Left e => pure $ fail $ "client couldn't get map description, error:\n" ++ e
      Right map_description => with ST do
        settings <- queryPClient client settings {s=Disconnected}
        -- characterId <- queryPClient client characterId {s=Disconnected}
        rendering <- startRendering
          (renderingSettings settings) (background map_description) preload
        loadMap rendering map_description
        follow rendering characterId
        [pclient, ui, sdl] <- split client
        session_data <- new $ MkSessionData characterId
        combine client [pclient, session_data, rendering, ui, sdl]
        pure $ Right ()

  disconnect client = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    delete session_data
    endRendering rendering
    combine client [pclient, ui, sdl]

  addObject client id ref = with ST do
    preload <- queryPClient client preload {s=Connected}
    case getObjectDescription ref preload of
      Left e => lift $ log $ "couldn't get object description, error:\n " ++ e
      Right object_description => with ST do
        [pclient, session_data, rendering, ui, sdl] <- split client
        addObject rendering id object_description
        combine client [pclient, session_data, rendering, ui, sdl]

  removeObject client id = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    Rendering.removeObject rendering id
    combine client [pclient, session_data, rendering, ui, sdl]

  runCommand client command = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    Rendering.runCommand rendering command
    combine client [pclient, session_data, rendering, ui, sdl]

  runCommands client [] = pure ()
  runCommands client (cmd::xs)
    = runCommand client cmd >>= const (runCommands client xs)

  runClientCommand client (Stop (Zoom x)) = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    zoom rendering x
    combine client [pclient, session_data, rendering, ui, sdl]
  runClientCommand client (Stop MainMenu) = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    toggleRoot ui "main/ui/main_menu.json" 100 100
    combine client [pclient, session_data, rendering, ui, sdl]
  runClientCommand client _ = pure ()

  runClientCommands client [] = pure ()
  runClientCommands client (cmd::xs)
    = runClientCommand client cmd >>= const (runClientCommands client xs)

  runServerCommand client (Create id ref) = addObject client id ref
  runServerCommand client (Destroy id) = removeObject client id
  runServerCommand client (Control cmd)
    = case getId cmd == !(querySessionData client characterId) of
        False => runCommand client cmd
        True => pure ()
  runServerCommand client (UpdateNumericProperty object_id prop_id current)
    = updatePRendering client $ prenderingUpdateNumericProperty object_id prop_id current

  runServerCommands client [] = pure ()
  runServerCommands client (cmd::xs)
    = runServerCommand client cmd >>= const (runServerCommands client xs)

  iterate client bodyData = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    updateBodyData rendering bodyData
    clear sdl
    render rendering sdl
    renderUI ui sdl
    present sdl
    sdl_events <- poll
    camera <- getCamera rendering
    combine client [pclient, session_data, rendering, ui, sdl]
    characterId <- querySessionData client characterId
    case processEvents characterId camera sdl_events of
      Left _ => pure $ Left ()
      Right (clientCommands, commands) => with ST do
        runClientCommands client clientCommands
        runCommands client commands
        pure $ Right commands

  updatePRendering client f = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    Rendering.updatePRendering rendering f
    combine client [pclient, session_data, rendering, ui, sdl]

  refreshSettings client = with ST do
    [pclient, session_data, rendering, ui, sdl] <- split client
    renderingSettings <- Rendering.getSettings rendering
    combine client [pclient, session_data, rendering, ui, sdl]
    updatePClient client {s=Connected} $ updateSettings $ setRenderingSettings renderingSettings

  getSettings client = with ST do
    refreshSettings client
    queryPClient client settings {s=Connected}

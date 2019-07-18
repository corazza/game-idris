module Client

import Control.ST
import Control.ST.ImplicitCall

import Client.Rendering
import Client.Rendering.Camera
import Client.PClient
import Client.SDL
import Client.Input
import Server.PServer
import Descriptions
import Descriptions.ObjectDescription.RenderDescription
import Dynamics
import Dynamics.PDynamics
import JSONCache
import GameIO
import Settings
import Exception
import Objects
import Commands

public export
interface Client (m : Type -> Type) where
  SClient : Type

  startClient : (settings : ClientSettings) ->
                (map : ContentReference) ->
                (preload : PreloadResults) ->
                (characterId : ObjectId) ->
                (initialCommands : List ServerCommand) ->
                ST m (Checked Var) [addIfRight SClient]
  endClient : (client : Var) -> ST m () [remove client SClient]

  queryPClient : (client : Var) -> (q : PClient -> a) -> ST m a [client ::: SClient]

  -- processes server commands and strips own controls NETWORKING
  runServerCommand : (client : Var) -> ServerCommand -> ST m () [client ::: SClient]
  runServerCommands : (client : Var) -> List ServerCommand -> ST m () [client ::: SClient]

  -- gets input, converts to own commands, processes them, returns for sending to server
  iterate : (client : Var) ->
            (bodyData : Objects BodyData) ->
            ST m (Either () (List Command)) [client ::: SClient]

  private
  runCommand : (client : Var) -> (command : Command) -> ST m () [client ::: SClient]
  private
  runCommands : (client : Var) -> (commands : List Command) -> ST m () [client ::: SClient]

  private
  addObject : (client : Var) -> (id : ObjectId) -> (ref : ContentReference) -> ST m () [client ::: SClient]

  private
  loadWalls : (client : Var) ->
              MapDescription ->
              ST m (Checked (List (WallCreation, ObjectDescription))) [client ::: SClient]

  private
  addWall : (client : Var) -> WallCreation -> ObjectDescription -> ST m () [client ::: SClient]
  private
  addWalls : (client : Var) -> List (WallCreation, ObjectDescription) -> ST m () [client ::: SClient]
  private
  loadMap : (client : Var) -> MapDescription -> ST m () [client ::: SClient]

export
(GameIO m, Rendering m, SDL m) => Client m where
  SClient = Composite [State PClient, SRendering {m}, SSDL {m}]

  startClient settings map_ref preload characterId initialCommands = with ST do
    case getMapDescription map_ref preload of
      Left e => pure $ fail $ "client couldn't get map description, error:\n" ++ e
      Right map_description => with ST do
        pclient <- new $ fromMapPreload characterId map_description preload
        rendering <- startRendering
          (renderingSettings settings) (background map_description) preload
        follow rendering characterId
        sdl <- startSDL (resolutionX settings) (resolutionY settings)
        client <- new ()
        combine client [pclient, rendering, sdl]
        loadMap client map_description
        runServerCommands client initialCommands
        pure (Right client)

  endClient client = with ST do
    [pclient, rendering, sdl] <- split client
    endRendering rendering
    endSDL sdl
    delete pclient
    delete client

  queryPClient client q = with ST do
    [pclient, rendering, sdl] <- split client
    pclient' <- read pclient
    combine client [pclient, rendering, sdl]
    pure $ q pclient'

  runCommand client command = with ST do
    [pclient, rendering, sdl] <- split client
    Rendering.runCommand rendering command
    combine client [pclient, rendering, sdl]

  runCommands client [] = pure ()
  runCommands client (cmd::xs)
    = runCommand client cmd >>= const (runCommands client xs)

  runServerCommand client (Create id ref) = addObject client id ref
  runServerCommand client (Destroy id) = with ST do
    [pclient, rendering, sdl] <- split client
    removeObject rendering id
    combine client [pclient, rendering, sdl]
  runServerCommand client (Control cmd)
    = case getId cmd == !(queryPClient client characterId) of
        False => runCommand client cmd
        True => pure ()
  runServerCommand client InfoUpdate = pure ()

  runServerCommands client [] = pure ()
  runServerCommands client (cmd::xs)
    = runServerCommand client cmd >>= const (runServerCommands client xs)

  iterate client bodyData = with ST do
    [pclient, rendering, sdl] <- split client
    updateBodyData rendering bodyData
    render rendering sdl
    sdl_events <- poll
    camera <- getCamera rendering
    combine client [pclient, rendering, sdl]
    case processEvents camera sdl_events of
      Left () => pure $ Left ()
      Right events => with ST do
        characterId <- queryPClient client characterId
        let commands = map (\x => x characterId) events
        runCommands client commands
        pure $ Right commands

  addObject client id ref = with ST do
    preload <- queryPClient client preload
    case getObjectDescription ref preload of
      Left e => lift $ log $ "couldn't get object description, error:\n " ++ e
      Right object_description => with ST do
        [pclient, rendering, sdl] <- split client
        addObject rendering id (render object_description) 1
        combine client [pclient, rendering, sdl]

  loadWalls client map_description
    = queryPClient client preload >>= pure . flip getWallsAsObjects map_description

  addWall client wall_creation object_description = with ST do
    [pclient, rendering, sdl] <- split client
    addObject rendering (id wall_creation) (render object_description) 0
    combine client [pclient, rendering, sdl]

  addWalls client [] = pure ()
  addWalls client ((creation, desc)::xs) = addWall client creation desc >>=
    const (addWalls client xs)

  loadMap client map_description = with ST do
    Right walls <- loadWalls client map_description | Left e => with ST do
      lift $ log $ "client couldn't get walls, error:"
      lift $ log e
    addWalls client walls

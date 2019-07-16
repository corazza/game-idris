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
                (map : ResourceReference) ->
                (preload : PreloadResults) ->
                (characterId : ObjectId) ->
                ST m (Checked Var) [addIfRight SClient]
  endClient : (client : Var) -> ST m () [remove client SClient]

  queryPClient : (client : Var) -> (q : PClient -> a) -> ST m a [client ::: SClient]

  -- processes server commands and strips own controls NETWORKING
  processServerCommand : (client : Var) -> ServerCommand -> ST m () [client ::: SClient]
  processServerCommands : (client : Var) -> List ServerCommand -> ST m () [client ::: SClient]

  -- gets input, converts to own commands, processes them, returns for sending to server
  iterate : (client : Var) ->
            (bodyData : Objects BodyData) ->
            ST m (Either () (List Command)) [client ::: SClient]

  private
  processCommand : (client : Var) -> (command : Command) -> ST m () [client ::: SClient]
  private
  processCommands : (client : Var) -> (commands : List Command) -> ST m () [client ::: SClient]

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

  startClient settings map_ref preload characterId = with ST do
    case getMapDescription map_ref preload of
      Left e => pure $ fail $ "client couldn't get map description, error:\n" ++ e
      Right map_description => with ST do
        pclient <- new $ fromMapPreload characterId map_description preload
        rendering <- startRendering (renderingSettings settings) (background map_description)
        sdl <- startSDL (resolutionX settings) (resolutionY settings)
        client <- new ()
        combine client [pclient, rendering, sdl]
        loadMap client map_description
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

  processCommand client command = with ST do
    [pclient, rendering, sdl] <- split client
    Rendering.processCommand rendering command
    combine client [pclient, rendering, sdl]

  processCommands client [] = pure ()
  processCommands client (cmd::xs)
    = processCommand client cmd >>= const (processCommands client xs)

  iterate client bodyData = with ST do
    [pclient, rendering, sdl] <- split client
    render rendering sdl bodyData
    sdl_events <- poll
    camera <- getCamera rendering
    combine client [pclient, rendering, sdl]
    case processEvents camera sdl_events of
      Left () => pure $ Left ()
      Right events => with ST do
        characterId <- queryPClient client characterId
        let commands = map (\x => x characterId) events
        processCommands client commands
        pure $ Right commands

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

module Server

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Server.PServer
import Dynamics
import Dynamics.PDynamics
import Commands
import GameIO
import Objects
import JSONCache
import Exception
import Descriptions
import Settings
import Exception

public export
interface Server (m : Type -> Type) where
  SServer : Type

  startServer : (settings : ServerSettings) ->
                (map_ref : ResourceReference) ->
                (preload : PreloadResults) ->
                ST m (Checked Var) [addIfRight SServer]

  endServer : (server : Var) -> ST m () [remove server SServer]

  queryPServer : (server : Var) -> (q : PServer -> a) -> ST m a [server ::: SServer]

  iterate : (server : Var) ->
            (dt : Int) ->
            ST m (List DynamicsCommand, List ServerCommand) [server ::: SServer]

  private
  loadWalls : (server : Var) ->
              MapDescription ->
              ST m (Checked (List (WallCreation, ObjectDescription))) [server ::: SServer]
  private
  loadObjects : (server : Var) ->
                MapDescription ->
                ST m (Checked (List (Creation, ObjectDescription))) [server ::: SServer]
  private
  addWall : (server : Var) -> WallCreation -> ObjectDescription -> ST m () [server ::: SServer]
  private
  addWalls : (server : Var) -> List (WallCreation, ObjectDescription) -> ST m () [server ::: SServer]
  private
  newId : (server : Var) -> ST m ObjectId [server ::: SServer]
  private
  addObject : (server : Var) -> Creation -> ObjectDescription -> ST m () [server ::: SServer]
  private
  addObjects : (server : Var) -> List (Creation, ObjectDescription) -> ST m () [server ::: SServer]
  private
  loadMap : (server : Var) -> MapDescription -> ST m () [server ::: SServer]

export
(GameIO m, Dynamics m) => Server m where
  SServer = State PServer

  startServer settings map_ref preload = with ST do
    case getMapDescription map_ref preload of
      Left e => pure $ fail $ "server couldn't get map description, error:\n" ++ e
      Right map_description => with ST do
        server <- new $ fromMapPreload map_description preload
        loadMap server map_description -- TODO this should also be able to fail
        pure (Right server)

  endServer server = delete server

  queryPServer server q = pure $ q !(read server)

  iterate server dt = with ST do
    dynamicsOutput <- queryPServer server dynamicsCommands
    clientOutput <- queryPServer server serverCommands
    update server flushCommands
    pure (dynamicsOutput, clientOutput)

  loadWalls server map_description
    = queryPServer server preload >>= pure . flip getWallsAsObjects map_description

  loadObjects server map_description
    = queryPServer server preload >>= pure . flip getObjectsFromMap map_description

  addWall server wall_creation object_description
    = update server $ addDynamicsCommand $ createWallCommand wall_creation object_description

  addWalls server [] = pure ()
  addWalls server ((creation, desc)::xs) = addWall server creation desc >>=
    const (addWalls server xs)

  newId server = with ST do
    id_num <- queryPServer server idCounter
    update server scounter
    pure $ "server_autoid_" ++ show id_num

  addObject server creation object_description = with ST do
    id <- newId server
    update server $ addDynamicsCommand $ createObjectCommand creation object_description id
    update server $ addClientCommand $ Create id (ref creation)

  addObjects server [] = pure ()
  addObjects server ((creation, desc)::xs) = addObject server creation desc >>=
    const (addObjects server xs)

  loadMap server map_description = with ST do
    Right walls <- loadWalls server map_description | Left e => with ST do
      lift $ log $ "server couldn't get walls, error:"
      lift $ log e
    Right objects <- loadObjects server map_description | Left e => with ST do
      lift $ log $ "couldn't get objects, error:"
      lift $ log e
    addWalls server walls
    addObjects server objects

  -- pruneOutside

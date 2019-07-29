module Main

import Control.ST
import Control.ST.ImplicitCall

import GameIO
import Commands
import JSONCache
import Descriptions
import Settings
import Timeline
import Exception
import Objects
import Dynamics
import Dynamics.PDynamics
import Dynamics.DynamicsEvent
import Server
import Server.PServer
import Server.Rules
import Client
import Client.Rendering
import Client.SDL
import Client.PClient

GameState : (GameIO m, Dynamics m, Client m, Server m) => ClientState -> Type
GameState Disconnected {m} = Composite [SDynamics {m},
                                        SServer {m},
                                        SClient Disconnected {m}]
GameState Connected {m} = Composite [SDynamics {m},
                           SServer {m},
                           SClient Connected {m},
                           State Int, -- lastms
                           State Int] -- carry

connect : (GameIO m, Dynamics m, Client m, Server m) =>
          (map_ref : ContentReference)


game : (GameIO m, Dynamics m, Client m, Server m) =>
       (preload : PreloadResults) ->
       (settings : GameSettings) ->
       (character : Character) ->
       ST m () []
game preload settings character = with ST do
  client <- startClient (client settings) character_id preload
  dynamics <- startDynamics (dynamics settings)
  let map_ref = map character
  Right server <- startServer (server settings) map_ref preload
        | Left e => with ST do
            lift $ log $ "couldn't start server, error: "
            lift $ log e
            endDynamics dynamics
  initialCommands <- getDynamicsCommands server
  runCommands dynamics initialCommands
  Right character_id <- login server character
        | Left e => with ST do
            lift $ log $ "couldn't log in, error:\n" ++ e
            endServer server
            endDynamics dynamics
  dynamicsCommands <- getDynamicsCommands server
  runCommands dynamics dynamicsCommands
  initialCommands <- getInSessionCommands server
  Right () <- connect client map_ref | Left e => with ST do
      lift $ log $ "couldn't start client, error:\n" ++ e
      endServer server
      endDynamics dynamics
      endClient client
  runServerCommands client initialCommands
  lastms <- new !ticks
  carry <- new 0
  state <- new ()
  combine state [dynamics, server, client, lastms, carry]
  loop state
  [dynamics, server, client, lastms, carry] <- split state
  delete state; delete carry; delete lastms
  disconnect client
  endClient client
  endServer server
  endDynamics dynamics

iterateCarry : GameIO m => Dynamics m => Server m =>
               (dynamics : Var) ->
               (server : Var) ->
               (time : Int) ->
               (characterId : ObjectId) ->
               ST m (Int, List InSession) [dynamics ::: SDynamics {m}, server ::: SServer {m}]
iterateCarry dynamics server time characterId = with ST do
  dt <- queryPDynamics dynamics timeStep
  case time > dt of
    False => with ST do
      serverCommands <- getInSessionCommands server
      pure (time, serverCommands)
    True => with ST do
      dynamicsEvents <- iterate dynamics
      bodyData <- queryPDynamics dynamics objects
      dynamicsCommands <- iterate server dynamicsEvents bodyData
      runCommands dynamics $ filterControl characterId dynamicsCommands
      if time `div` dt < 8
        then iterateCarry dynamics server (time - dt) characterId
        else with ST do
          serverCommands <- getInSessionCommands server
          pure (0, serverCommands)

runSessionCommands : (GameIO m, Dynamics m, Client m, Server m) =>
                     (state : Var) ->
                     (sessionCommands : List SessionCommand) ->
                     ST m Bool [state ::: GameState Connected {m} :->
                            \res => if res then GameState Connected {m}
                                           else GameState Disconnected {m}]

loop : (GameIO m, Dynamics m, Client m, Server m) =>
       (state : Var) ->
       ST m () [state ::: GameState Connected {m}]
loop state = with ST do
  beforems <- ticks
  [dynamics, server, client, lastms, carry] <- split state
  let passed = beforems - !(read lastms)
  bodyData <- queryPDynamics dynamics objects

  Right commands <- iterate client bodyData
        | Left () => combine state [dynamics, server, client, lastms, carry]

  receiveClientCommands server commands
  runCommands dynamics $ catMaybes $ map fromCommand commands

  characterId <- queryPClient client characterId
  let time = passed + !(read carry)
  (newCarry, serverCommands) <- iterateCarry dynamics server time characterId

  runServerCommands client serverCommands
  sessionCommands <- getSessionCommands server

  write carry newCarry
  write lastms beforems

  combine state [dynamics, server, client, lastms, carry]

  logout <- runSessionCommands state sessionCommands

  case logout of
       False => ?sdfgsdfs_1
       True => ?sdfgsdfs_2

  -- loop state

start : (GameIO m, Dynamics m, Client m, Server m) => ST m () []
start = with ST do
  Right dynamics_settings <- lift $ loadDynamicsSettings
      | Left e => lift (log e)
  Right server_settings <- lift $ loadServerSettings
      | Left e => lift (log e)
  Right client_settings <- lift $ loadClientSettings
      | Left e => lift (log e)
  let settings = MkGameSettings dynamics_settings server_settings client_settings
  Right timeline <- lift $ loadTimeline "saves/default.json"
      | Left e => lift (log e)
  let characterId = character timeline
  case lookup characterId (characters timeline) of
    Nothing => with ST do
      lift $ log $ "can't find character with id \"" ++ characterId ++ "\""
    Just character => with ST do
      Right preload_info <- lift $ checkedJSONLoad {r=Preload} "res/main/preload.json"
            | Left e => with ST do
                  lift $ log "couldn't load preload info, error:"
                  lift $ log e
      Right preload <- preloadResults preload_info
            | Left e => with ST do
                  lift $ log "couldn't load preload, error:"
                  lift $ log e
      game preload settings character

main : IO ()
main = with IO do
  disableBuffering
  run start

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

-- HERE the server handles a map, and characters on that map
-- rules has a store of characters, functions like Equip modify them
-- Rules.updateCharacter: updates rules version, outputs a RulesOutput.UpdateCharacter
-- this is converted by the server into a SessionCommand

record PGame where
  constructor MkPGame
  settings : GameSettings
  preload : PreloadResults
  timeline : Timeline
  characterId : CharacterId
  character : Character

updateSettings : (f : GameSettings -> GameSettings) -> PGame -> PGame
updateSettings f = record { settings $= f }

updateTimeline : (f : Timeline -> Timeline) -> PGame -> PGame
updateTimeline f = record { timeline $= f }

pgameUpdateCharacter : (character_id : CharacterId) ->
                       (f : Character -> Character) ->
                       PGame -> PGame
pgameUpdateCharacter character_id f pgame
  = let timelineUpdate = updateTimeline $ updateCharacter character_id f
        in case character_id == characterId pgame of
                True => ( (record {character $= f}) . timelineUpdate) pgame
                False => timelineUpdate pgame
  -- = ( record { character $= f }
  -- .  (updateTimeline $ updateCharacter (characterId pgame) f) ) pgame

record GameSessionData where
  constructor MkGameSessionData
  lastms : Int
  carry : Int

fromTicks : Int -> GameSessionData
fromTicks ticks = MkGameSessionData ticks 0

GameState : (GameIO m, Dynamics m, Client m, Server m) => ClientState -> Type
GameState Disconnected {m} = Composite [State PGame, SClient Disconnected {m}]
GameState Connected {m} = Composite
 [ State PGame,
   SClient Connected {m},
   SDynamics {m},
   SServer {m},
   State GameSessionData ]

queryPGame : (GameIO m, Dynamics m, Client m, Server m) =>
             (state : Var) ->
             (q : PGame -> a) ->
             ST m a [state ::: GameState s {m}]
queryPGame state q {s} = case s of
  Disconnected => with ST do
    [pgame, client] <- split state
    pgame' <- read pgame
    combine state [pgame, client]
    pure $ q pgame'
  Connected => with ST do
    [pgame, client, dynamics, server, game_session_data] <- split state
    pgame' <- read pgame
    combine state [pgame, client, dynamics, server, game_session_data]
    pure $ q pgame'

updatePGame : (GameIO m, Dynamics m, Client m, Server m) =>
              (state : Var) ->
              (f : PGame -> PGame) ->
              ST m () [state ::: GameState s {m}]
updatePGame state f {s} = case s of
  Disconnected => with ST do
    [pgame, client] <- split state
    update pgame f
    combine state [pgame, client]
  Connected => with ST do
    [pgame, client, dynamics, server, game_session_data] <- split state
    update pgame f
    combine state [pgame, client, dynamics, server, game_session_data]

queryGameSessionData : (GameIO m, Dynamics m, Client m, Server m) =>
                       (state : Var) ->
                       (q : GameSessionData -> a) ->
                       ST m a [state ::: GameState Connected {m}]
queryGameSessionData state q = with ST do
  [pgame, client, dynamics, server, game_session_data] <- split state
  game_session_data' <- read game_session_data
  combine state [pgame, client, dynamics, server, game_session_data]
  pure $ q game_session_data'

updateGameSessionData : (GameIO m, Dynamics m, Client m, Server m) =>
                       (state : Var) ->
                       (f : GameSessionData -> GameSessionData) ->
                       ST m () [state ::: GameState Connected {m}]
updateGameSessionData state f = with ST do
  [pgame, client, dynamics, server, game_session_data] <- split state
  update game_session_data f
  combine state [pgame, client, dynamics, server, game_session_data]

startSession : (GameIO m, Dynamics m, Client m, Server m) =>
               (state : Var) ->
               (map_ref : ContentReference) ->
               ST m (Checked ()) [state ::: GameState Disconnected {m} :->
                                    \res => if isRight res
                                              then GameState Connected {m}
                                              else GameState Disconnected {m}]
startSession state map_ref = with ST do
  [pgame, client] <- split state
  pgame' <- read pgame
  let settings = settings pgame'
  let preload = PGame.preload pgame'
  let character = PGame.character pgame'
  let character_id = characterId pgame'
  dynamics <- startDynamics (dynamics settings)
  Right server <- startServer (server settings) map_ref preload | Left e => with ST do
            endDynamics dynamics
            combine state [pgame, client]
            pure $ fail $ "couldn't start server, error: " ++ e
  initialCommands <- getDynamicsCommands server
  runCommands dynamics initialCommands
  Right character_object_id <- login server character_id character | Left e => with ST do
            endServer server
            endDynamics dynamics
            combine state [pgame, client]
            pure $ fail $ "couldn't log in, error:\n" ++ e
  dynamicsCommands <- getDynamicsCommands server
  runCommands dynamics dynamicsCommands
  initialCommands <- getInSessionCommands server
  Right () <- Client.connect client map_ref character_object_id | Left e => with ST do
      endServer server
      endDynamics dynamics
      combine state [pgame, client]
      pure $ fail $ "couldn't connect client, error:\n" ++ e
  runServerCommands client initialCommands
  game_session_data <- new $ fromTicks !ticks
  combine state [pgame, client, dynamics, server, game_session_data]
  pure $ Right ()

save : (GameIO m, Dynamics m, Client m, Server m) =>
       (state : Var) ->
       ST m () [state ::: GameState Connected {m}]
save state = with ST do
  [pgame, client, dynamics, server, game_session_data] <- split state
  clientSettings <- getSettings client
  combine state [pgame, client, dynamics, server, game_session_data]
  updatePGame state {s=Connected} $ updateSettings $ setClientSettings clientSettings
  newSettings <- queryPGame state settings {s=Connected}
  lift $ saveSettings newSettings
  characterId' <- queryPGame state characterId {s=Connected}
  character <- queryPGame state character {s=Connected}
  updatePGame state {s=Connected} $ updateTimeline $ updateCharacter characterId' $ const character
  timeline <- queryPGame state timeline {s=Connected}
  lift $ saveTimeline "saves/one.json" timeline

endSession : (GameIO m, Dynamics m, Client m, Server m) =>
             (state : Var) ->
             ST m () [state ::: GameState Connected {m} :-> GameState Disconnected {m}]
endSession state = with ST do
  save state
  [pgame, client, dynamics, server, game_session_data] <- split state
  delete game_session_data
  disconnect client
  endServer server
  endDynamics dynamics
  combine state [pgame, client]

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

runSessionCommand : (GameIO m, Dynamics m, Client m, Server m) =>
                    (state : Var) ->
                    (sessionCommand : SessionCommand) ->
                    ST m (Either ContentReference ()) [state ::: GameState Connected {m}]
runSessionCommand state (Relog id to) = with ST do
  [pgame, client, dynamics, server, game_session_data] <- split state
  character_object_id <- querySessionData client characterId
  combine state [pgame, client, dynamics, server, game_session_data]
  case id == character_object_id of
    True => pure $ Left to
    False => pure $ Right ()

runSessionCommands : (GameIO m, Dynamics m, Client m, Server m) =>
                     (state : Var) ->
                     (sessionCommands : List SessionCommand) ->
                     ST m (Either ContentReference ()) [state ::: GameState Connected {m}]
runSessionCommands state [] = pure $ Right ()
runSessionCommands state (cmd::xs) = case !(runSessionCommand state cmd) of
  Left to => pure $ Left to
  Right () => runSessionCommands state xs

runGameCommand : (GameIO m, Dynamics m, Client m, Server m) =>
                 (state : Var) ->
                 (gameCommand : GameCommand) ->
                 ST m () [state ::: GameState Connected {m}]
runGameCommand state (UpdateCharacter character_id f)
  = updatePGame state {s=Connected} $ pgameUpdateCharacter character_id f

runGameCommands : (GameIO m, Dynamics m, Client m, Server m) =>
                  (state : Var) ->
                  (gameCommands : List GameCommand) ->
                  ST m () [state ::: GameState Connected {m}]
runGameCommands state [] = pure ()
runGameCommands state (cmd::xs)
  = runGameCommand state cmd >>= const (runGameCommands state xs)

data LoopResult = Exit | Relog ContentReference

loop : (GameIO m, Dynamics m, Client m, Server m) =>
       (state : Var) ->
       ST m LoopResult [state ::: GameState Connected {m}]
loop state = with ST do
  beforems <- ticks
  [pgame, client, dynamics, server, game_session_data] <- split state
  game_session_data' <- read game_session_data
  let passed = beforems - (lastms game_session_data')
  bodyData <- queryPDynamics dynamics objects

  Right commands <- iterate client bodyData | Left () => with ST do
      combine state [pgame, client, dynamics, server, game_session_data]
      pure Exit

  receiveClientCommands server commands
  runCommands dynamics $ catMaybes $ map fromCommand commands

  characterId <- querySessionData client characterId
  let time = passed + (carry game_session_data')
  (newCarry, serverCommands) <- iterateCarry dynamics server time characterId

  runServerCommands client serverCommands

  write game_session_data $ MkGameSessionData beforems newCarry

  sessionCommands <- getSessionCommands server
  gameCommands <- getGameCommands server

  combine state [pgame, client, dynamics, server, game_session_data]

  runGameCommands state gameCommands

  logout <- runSessionCommands state sessionCommands

  case logout of
       Left to => pure $ Relog to
       Right () => loop state

game : (GameIO m, Dynamics m, Client m, Server m) =>
       (state : Var) ->
       ST m () [state ::: GameState Disconnected {m}]
game state = with ST do
  map_ref <- queryPGame state {s=Disconnected} $ (map . character)
  characterId <- queryPGame state {s=Disconnected} $ characterId
  Right () <- startSession state map_ref | Left e => with ST do
    lift $ log $ "couldn't start session, error:\n" ++ e
  case !(loop state) of
    Exit => endSession state
    Relog to => with ST do
      updatePGame state {s=Connected} $ pgameUpdateCharacter characterId $ setMap to
      endSession state
      game state

start : (GameIO m, Dynamics m, Client m, Server m) => ST m () []
start = with ST do
  Right dynamics_settings <- lift $ loadDynamicsSettings
      | Left e => lift (log e)
  Right server_settings <- lift $ loadServerSettings
      | Left e => lift (log e)
  Right client_settings <- lift $ loadClientSettings
      | Left e => lift (log e)
  let settings = MkGameSettings dynamics_settings server_settings client_settings
  Right timeline <- lift $ loadTimeline "saves/one.json"
      | Left e => lift (log e)
  let characterId = character timeline
  case getCharacter characterId timeline of
    Nothing => lift $ log $ "couldn't get character with id " ++ characterId
    Just character => with ST do
      Right preload_info <- lift $ checkedJSONLoad {r=Preload} "res/main/preload.json"
            | Left e => with ST do
                  lift $ log "couldn't load preload info, error:"
                  lift $ log e
      Right preload <- preloadResults preload_info
            | Left e => with ST do
                  lift $ log "couldn't load preload, error:"
                  lift $ log e
      pgame <- new $ MkPGame settings preload timeline characterId character
      client <- startClient (client settings) preload
      state <- new ()
      combine state [pgame, client]
      game state
      [pgame, client] <- split state
      endClient client
      delete pgame
      delete state

main : IO ()
main = with IO do
  disableBuffering
  run start

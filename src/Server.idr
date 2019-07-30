module Server

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Server.PServer
import Server.Rules
import Server.Rules.PRules
import Server.Rules.RulesOutput
import Dynamics
import Dynamics.PDynamics
import Dynamics.DynamicsEvent
import Commands
import GameIO
import Objects
import JSONCache
import Exception
import Descriptions.MapDescription
import Descriptions.ObjectDescription
import Descriptions.JointDescription
import Descriptions.ObjectDescription.RulesDescription
import Settings
import Exception
import Timeline

public export
interface Server (m : Type -> Type) where
  SServer : Type

  startServer : (settings : ServerSettings) ->
                (map_ref : ContentReference) ->
                (preload : PreloadResults) ->
                ST m (Checked Var) [addIfRight SServer]

  endServer : (server : Var) -> ST m () [remove server SServer]

  queryPServer : (server : Var) -> (q : PServer -> a) -> ST m a [server ::: SServer]
  updatePServer : (server : Var) -> (f : PServer -> PServer) -> ST m () [server ::: SServer]

  receiveClientCommand : (server : Var) -> Command -> ST m () [server ::: SServer]
  receiveClientCommands : (server : Var) -> List Command -> ST m () [server ::: SServer]

  iterate : (server : Var) ->
            (dynamicsEvents : List DynamicsEvent) ->
            (bodyData : Objects BodyData) ->
            ST m (List DynamicsCommand) [server ::: SServer]

  getInSessionCommands : (server : Var) -> ST m (List InSession) [server ::: SServer]
  getSessionCommands : (server : Var) -> ST m (List SessionCommand) [server ::: SServer]
  -- TODO temporary, should be included in startServer, which should return
  -- Checked (Var, List DynamicsCommand)
  getDynamicsCommands : (server : Var) -> ST m (List DynamicsCommand) [server ::: SServer]

  login : (server : Var) -> Character -> ST m LoginResponse [server ::: SServer]

  private
  updateBodyData : (server : Var) -> Objects BodyData -> ST m () [server ::: SServer]

  private
  processClientCommands : (server : Var) -> ST m () [server ::: SServer]

  private
  rulesProcessDynamicsEvents : (server : Var) ->
                               List DynamicsEvent ->
                               ST m () [server ::: SServer]
  private
  processRulesCommand : (server : Var) -> Command -> ST m () [server ::: SServer]
  private
  processRulesOutput : (server : Var) -> RulesOutput -> ST m () [server ::: SServer]
  private
  processRulesOutputs : (server : Var) -> List RulesOutput -> ST m () [server ::: SServer]
  private
  getAndProcessRulesOutputs : (server : Var) -> ST m () [server ::: SServer]

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
  addRules : (server : Var) ->
             (id : ObjectId) ->
             (desc : ObjectDescription) ->
             (creation : Creation) ->
             ST m () [server ::: SServer]
  private
  removeRules : (server : Var) ->
                (id : ObjectId) ->
                ST m () [server ::: SServer]
  private -- TODO URGENT unify!!!
  create : (server : Var) -> Creation -> ST m (Checked ObjectId) [server ::: SServer]
  private
  createObject : (server : Var) -> Creation -> ObjectDescription -> ST m ObjectId [server ::: SServer]
  private
  createObjects : (server : Var) -> List (Creation, ObjectDescription) -> ST m () [server ::: SServer]
  private
  createJoint : (server : Var) -> JointDescription -> ST m () [server ::: SServer]
  private
  createJoints : (server : Var) -> List JointDescription -> ST m () [server ::: SServer]
  private
  decideId : (server : Var) -> Creation -> ST m ObjectId [server ::: SServer]
  private
  loadMap : (server : Var) -> MapDescription -> ST m () [server ::: SServer]

  private
  destroy : (server : Var) -> ObjectId -> ST m () [server ::: SServer]
  private
  destroyMany : (server : Var) -> List ObjectId -> ST m () [server ::: SServer]
  private
  pruneOutside : (server : Var) -> ST m () [server ::: SServer]

export
(GameIO m, Rules m) => Server m where
  SServer = Composite [State PServer, SRules {m}]

  startServer settings map_ref preload = with ST do
    case getMapDescription map_ref preload of
      Left e => pure $ fail $ "server couldn't get map description, error:\n" ++ e
      Right map_description => with ST do
        pserver <- new $ fromMapPreload map_description preload
        rules <- startRules preload
        server <- new ()
        combine server [pserver, rules]
        loadMap server map_description -- TODO this should also be able to fail
        pure (Right server)

  endServer server = with ST do
    [pserver, rules] <- split server
    endRules rules
    delete pserver
    delete server

  queryPServer server q = with ST do
    [pserver, rules] <- split server
    let res = q !(read pserver)
    combine server [pserver, rules]
    pure res

  updatePServer server f = with ST do
    [pserver, rules] <- split server
    update pserver f
    combine server [pserver, rules]

  receiveClientCommand server command = updatePServer server $ addClientCommand command

  receiveClientCommands server [] = pure ()
  receiveClientCommands server (cmd::xs) = receiveClientCommand server cmd >>=
    const (receiveClientCommands server xs)

  iterate server dynamicsEvents bodyData = with ST do
    updateBodyData server bodyData
    processClientCommands server
    rulesProcessDynamicsEvents server dynamicsEvents
    getAndProcessRulesOutputs server
    getDynamicsCommands server

  rulesProcessDynamicsEvents server dynamicsEvents = with ST do
    [pserver, rules] <- split server
    processDynamicsEvents rules dynamicsEvents
    combine server [pserver, rules]

  getInSessionCommands server = with ST do
    clientOutput <- queryPServer server serverCommands
    updatePServer server flushInSessionCommands
    pure clientOutput

  getSessionCommands server = with ST do
    clientOutput <- queryPServer server sessionCommands
    updatePServer server flushSessionCommands
    pure clientOutput

  getDynamicsCommands server = with ST do
    dynamicsOutput <- queryPServer server dynamicsCommands
    updatePServer server flushDynamicsCommands
    pure dynamicsOutput

  login server character = with ST do
    preload <- queryPServer server preload
    let ref = ref character
    case getObjectDescription ref preload of
      Left e => pure $ loginFail ref e
      Right character_object => with ST do
        spawn <- queryPServer server (spawn . mapData)
        id <- createObject server (forCharacter spawn character) character_object
        updatePServer server $ addLoggedIn id character
        pure $ loginSuccess id

  updateBodyData server bodyData = with ST do
    updatePServer server $ pserverSetBodyData bodyData
    [pserver, rules] <- split server
    updatePRules rules $ prulesSetBodyData bodyData
    combine server [pserver, rules]

  processClientCommands server = with ST do
    clientCommands <- queryPServer server clientCommands
    updatePServer server flushInput
    updatePServer server $ addDynamicsCommands $ catMaybes $ map fromCommand clientCommands
    [pserver, rules] <- split server
    runCommands rules clientCommands
    combine server [pserver, rules]

  processRulesCommand server command = with ST do
    updatePServer server $ addInSessionCommand $ Control command
    case fromCommand command of
      Nothing => pure ()
      Just dyncom => updatePServer server $ addDynamicsCommand $ dyncom

  processRulesOutput server (Create creation) = do create server creation; pure ()
  processRulesOutput server (RuleCommand command) = processRulesCommand server command
  processRulesOutput server (Death id) = destroy server id
  processRulesOutput server (NumericPropertyCurrent object_id prop_id current)
    = updatePServer server $ addInSessionCommand $ UpdateNumericProperty object_id prop_id current
  processRulesOutput server (ExitTo object_id ref)
    = updatePServer server $ addSessionCommand $ Relog object_id ref
     -- TODO update logged in

  processRulesOutputs server [] = pure ()
  processRulesOutputs server (rule_output::xs)
    = processRulesOutput server rule_output >>= const (processRulesOutputs server xs)

  getAndProcessRulesOutputs server = with ST do
    [pserver, rules] <- split server
    rules_output <- getRulesOutputs rules
    combine server [pserver, rules]
    processRulesOutputs server rules_output

  loadWalls server map_description
    = queryPServer server preload >>= pure . flip getWallsAsObjects map_description

  loadObjects server map_description
    = queryPServer server preload >>= pure . flip getObjectsFromMap map_description

  addWall server wall_creation object_description
    = updatePServer server
    $ addDynamicsCommand
    $ createWallCommand wall_creation object_description

  addWalls server [] = pure ()
  addWalls server ((creation, desc)::xs) = addWall server creation desc >>=
    const (addWalls server xs)

  newId server = with ST do
    id_num <- queryPServer server idCounter
    updatePServer server scounter
    pure $ "server_autoid_" ++ show id_num

  addRules server id object_description creation
    = case rulesDescFromCreation (rules object_description) creation of
        Nothing => pure ()
        Just rules_description => with ST do
          [pserver, rules] <- split server
          addObject rules id rules_description (creator creation)
          combine server [pserver, rules]

  removeRules server id = with ST do
    [pserver, rules] <- split server
    removeObject rules id
    combine server [pserver, rules]

  create server creation = with ST do
    preload <- queryPServer server preload
    case getObjectDescription (ref creation) preload of
      Left e => with ST do
        lift $ log $ "can't create " ++ ref creation ++ ", error:\n" ++ e
        pure $ fail e
      Right object_description =>
        createObject server creation object_description >>= pure . Right

  decideId server creation = case Creation.id creation of
    Nothing => newId server
    Just id' => with ST do
      bodyData <- queryPServer server bodyData
      case hasKey id' bodyData of
        False => pure id'
        True => newId server

  createObject server creation object_description = with ST do
    id <- decideId server creation
    updatePServer server $ addDynamicsCommand $ createObjectCommand creation object_description id
    updatePServer server $ addInSessionCommand $ Create id (ref creation)
    addRules server id object_description creation
    pure id

  createObjects server [] = pure ()
  createObjects server ((creation, desc)::xs) = createObject server creation desc >>=
    const (createObjects server xs)

  createJoint server desc
    = updatePServer server $ addDynamicsCommand $ CreateJoint !(newId server) desc

  createJoints server [] = pure ()
  createJoints server (desc::xs)
    = createJoint server desc >>= const (createJoints server xs)

  loadMap server map_description = with ST do
    Right walls <- loadWalls server map_description | Left e => with ST do
      lift $ log $ "server couldn't get walls, error:"
      lift $ log e
    Right objects <- loadObjects server map_description | Left e => with ST do
      lift $ log $ "couldn't get objects, error:"
      lift $ log e
    addWalls server walls
    createObjects server objects
    createJoints server (joints map_description)

  destroy server id = with ST do
    updatePServer server $ addDynamicsCommand $ PDynamics.Destroy id
    updatePServer server $ addInSessionCommand $ PServer.Destroy id
    removeRules server id

  destroyMany server [] = pure ()
  destroyMany server (id::xs) = destroy server id >>= const (destroyMany server xs)

  pruneOutside server = with ST do
    dimensions <- queryPServer server pserverGetDimensions
    bodyData <- queryPServer server bodyData
    let outside = getOutside dimensions (toList bodyData)
    destroyMany server outside

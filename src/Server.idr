module Server

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Server.PServer
import Server.Rules
import Server.Rules.PRules
import Server.Rules.RulesOutput
import Client.ClientCommands
import Dynamics.BodyData
import Dynamics.DynamicsEvent
import Dynamics.DynamicsCommand
import Dynamics.MoveDirection
import Commands
import GameIO
import Objects
import JSONCache
import Exception
import Descriptions.MapDescription
import Descriptions.JointDescription
import Descriptions.ObjectDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RenderDescription
import Settings
import Exception
import Timeline

-- TODO FIX the scope of the server is currently a single map
-- has to be changed to a 'timeline'

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
  getGameCommands : (server : Var) -> ST m (List GameCommand) [server ::: SServer]
  -- TODO temporary, should be included in startServer, which should return
  -- Checked (Var, List DynamicsCommand)
  getDynamicsCommands : (server : Var) -> ST m (List DynamicsCommand) [server ::: SServer]

  login : (server : Var) -> CharacterId -> Character -> ST m LoginResponse [server ::: SServer]

  private
  rulesAddCharacter : (server : Var) ->
                       (id : ObjectId) ->
                       (character_id : CharacterId) ->
                       (character : Character) ->
                       ST m () [server ::: SServer]

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
  loadStatic : (server : Var) ->
               MapDescription ->
               ST m (Checked (List (StaticCreation, ObjectDescription))) [server ::: SServer]

  private
  loadObjects : (server : Var) ->
                MapDescription ->
                ST m (Checked (List (Creation, ObjectDescription))) [server ::: SServer]
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
  createObject : (server : Var) ->
                 Creation ->
                 ObjectDescription ->
                 ST m (Checked ObjectId) [server ::: SServer]
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

  getGameCommands server = with ST do
    gameCommands <- queryPServer server gameCommands
    updatePServer server flushGameCommands
    pure gameCommands

  getDynamicsCommands server = with ST do
    dynamicsOutput <- queryPServer server dynamicsCommands
    updatePServer server flushDynamicsCommands
    pure dynamicsOutput

  rulesAddCharacter server id character_id character = with ST do
    [pserver, rules] <- split server
    addCharacter rules id character_id character
    combine server [pserver, rules]

  login server character_id character = with ST do
    loggedIn <- queryPServer server loggedIn
    case hasKey character_id loggedIn of
      True => pure $ loginFail character_id "already logged in"
      False => with ST do
        preload <- queryPServer server preload
        let ref = ref character
        case getObjectDescription ref preload of
          Left e => pure $ loginFail character_id e
          Right character_object => with ST do
            spawn <- queryPServer server (spawn . mapData)
            case !(createObject server (forCharacter spawn character) character_object) of
              Left e => pure $ loginFail character_id e
              Right id => with ST do
                updatePServer server $ addLoggedIn character_id id
                rulesAddCharacter server id character_id character
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
  processRulesOutput server (RunQuery id name span)
    = updatePServer server $ addDynamicsCommand $ QueryFor id name span
  processRulesOutput server (ApplyImpulse id impulse)
    = updatePServer server $ addDynamicsCommand $ ApplyImpulse id impulse
  processRulesOutput server (SetFacing id direction)
    = updatePServer server $ addDynamicsCommand $ SetFacing id direction
  processRulesOutput server (RuleCommand command) = processRulesCommand server command
  processRulesOutput server (Death id) = destroy server id
  processRulesOutput server (NumericPropertyCurrent object_id prop_id current)
    = updatePServer server $ addInSessionCommand $ UpdateNumericProperty object_id prop_id current
  processRulesOutput server (ExitTo object_id ref)
    = updatePServer server $ addSessionCommand $ Relog object_id ref
  processRulesOutput server (UpdateCharacter character_id f)
    = updatePServer server $ addGameCommand $ UpdateCharacter character_id f
  processRulesOutput server (RulesClientCommand character_id cmd)
    = updatePServer server $ addGameCommand $ RulesClientCommand character_id cmd
  processRulesOutput server (SetMaskBits object_id xs)
    = updatePServer server $ addDynamicsCommand $ SetMaskBits object_id xs
  processRulesOutput server (UnsetMaskBits object_id xs)
    = updatePServer server $ addDynamicsCommand $ UnsetMaskBits object_id xs
  processRulesOutput server (SetAttackShowing object_id ref)
    = updatePServer server $ addInSessionCommand $ SetAttackShowing object_id ref
  processRulesOutput server (UnsetAttackShowing object_id)
    = updatePServer server $ addInSessionCommand $ UnsetAttackShowing object_id
  processRulesOutput server (PlaySound ref)
    = updatePServer server $ addInSessionCommand $ PlaySound ref
  processRulesOutput server cmd =
    lift $ log $ "unimplemented handler in processRulesOutput for: " ++ show cmd
     -- TODO update logged in

  processRulesOutputs server [] = pure ()
  processRulesOutputs server (rule_output::xs)
    = processRulesOutput server rule_output >>= const (processRulesOutputs server xs)

  getAndProcessRulesOutputs server = with ST do
    [pserver, rules] <- split server
    rules_output <- getRulesOutputs rules
    combine server [pserver, rules]
    processRulesOutputs server rules_output

  loadStatic server map_description
    = queryPServer server preload >>= pure . flip getStaticFromMap map_description

  loadObjects server map_description
    = queryPServer server preload >>= pure . flip getObjectsFromMap map_description

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
        createObject server creation object_description

  decideId server creation = case Creation.id creation of
    Nothing => newId server
    Just id' => with ST do
      bodyData <- queryPServer server bodyData
      case hasKey id' bodyData of
        False => pure id'
        True => newId server

  createObject server creation object_description = with ST do
    id <- decideId server creation
    case createObjectCommand creation object_description id of
      Left e => pure $ fail $ e ++ " (createObject server)"
      Right command => with ST do
        updatePServer server $ addDynamicsCommand command
        updatePServer server $ addInSessionCommand $ Create id (ref creation) (render creation)
        addRules server id object_description creation
        pure $ Right id

  createObjects server [] = pure ()
  createObjects server ((creation, desc)::xs) = createObject server creation desc >>=
    const (createObjects server xs)

  createJoint server desc
    = updatePServer server $ addDynamicsCommand $ CreateJoint !(newId server) desc

  createJoints server [] = pure ()
  createJoints server (desc::xs)
    = createJoint server desc >>= const (createJoints server xs)

  loadMap server map_description = with ST do
    Right static <- loadStatic server map_description | Left e => with ST do
      lift $ log $ "server couldn't get static, error:\n" ++ e
    Right objects <- loadObjects server map_description | Left e => with ST do
      lift $ log $ "couldn't get objects, error:\n" ++ e
    createObjects server $ processStaticCreations static
    createObjects server objects
    createJoints server (joints map_description)

  destroy server id = with ST do
    updatePServer server $ addDynamicsCommand $ DynamicsCommand.Destroy id
    updatePServer server $ addInSessionCommand $ PServer.Destroy id
    removeRules server id

  destroyMany server [] = pure ()
  destroyMany server (id::xs) = destroy server id >>= const (destroyMany server xs)

  pruneOutside server = with ST do
    dimensions <- queryPServer server pserverGetDimensions
    bodyData <- queryPServer server bodyData
    let outside = getOutside dimensions (toList bodyData)
    destroyMany server outside

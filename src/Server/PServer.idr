module Server.PServer

import Physics.Vector2D

import Client.ClientCommands
import Dynamics.BodyData
import Dynamics.DynamicsCommand
import Descriptions.MapDescription
import GameIO
import Objects
import Commands
import Exception
import JSONCache
import Timeline

public export
record MapData where
  constructor MkMapData
  mapName : String
  dimensions : Vector2D -- dimensions of current map
  background : Background
  spawn : Vector2D

mapDescriptionToMapData : MapDescription -> MapData
mapDescriptionToMapData desc = MkMapData (name desc) (dimensions desc) (background desc) (spawn desc)

-- these are intended to be sent over the network, dynamics commands are always local
-- which means that a separate client dynamics system will need an additional
-- BodyData parameter in Create

public export
data InSession
  = Create ObjectId ContentReference -- client gets other parameters from dynamics
  | Destroy ObjectId
  | Control Command
  | UpdateNumericProperty ObjectId NumericPropertyId Double

export
Show InSession where
  show (Create id ref) = "create " ++ id ++ " " ++ ref
  show (Destroy id) = "destroy " ++ id
  show (Control cmd) = "control " ++ show cmd
  show (UpdateNumericProperty object_id prop_id current) = "info update"

public export
data SessionCommand
  = Relog ObjectId ContentReference

export
Show SessionCommand where
  show (Relog x y) = "relog " ++ x ++ " to " ++ y

-- things which would otherwise be on the server, so currently main receives them
public export
data GameCommand
  = UpdateCharacter CharacterId (Character -> Character)
  | RulesClientCommand CharacterId ClientCommand

public export
LoginResponse : Type
LoginResponse = Checked ObjectId

export
loginFail : (name : String) -> (error : String) -> LoginResponse
loginFail name error = fail $ "couldn't login " ++ name ++ ", error: " ++ error

export -- TODO URGENT unify with Server.createObject
loginSuccess : (id : ObjectId) -> LoginResponse
loginSuccess id = pure id

public export
record PServer where
  constructor MkPServer
  idCounter : Nat
  preload : PreloadResults
  mapData : MapData
  loggedIn : Dict CharacterId ObjectId
  bodyData : Objects BodyData
  dynamicsCommands : List DynamicsCommand -- output
  serverCommands : List InSession -- output
  sessionCommands : List SessionCommand -- output
  gameCommands : List GameCommand
  clientCommands : List Command -- input

export
scounter : PServer -> PServer
scounter = record { idCounter $= S }

export
fromMapPreload : MapDescription -> PreloadResults -> PServer
fromMapPreload desc preload = MkPServer
  0 preload (mapDescriptionToMapData desc) empty empty empty empty empty empty empty

export
addLoggedIn : CharacterId -> ObjectId -> PServer -> PServer
addLoggedIn character id = record { loggedIn $= insert character id }

export
addDynamicsCommand : DynamicsCommand -> PServer -> PServer
addDynamicsCommand cmd = record { dynamicsCommands $= append cmd }

export
addDynamicsCommands : List DynamicsCommand -> PServer -> PServer
addDynamicsCommands cmds = record { dynamicsCommands $= \xs => xs ++ cmds }

export
addInSessionCommand : InSession -> PServer -> PServer
addInSessionCommand cmd = record { serverCommands $= append cmd }

export
addSessionCommand : SessionCommand -> PServer -> PServer
addSessionCommand cmd = record { sessionCommands $= append cmd }

export
addGameCommand : GameCommand -> PServer -> PServer
addGameCommand cmd = record { gameCommands $= append cmd }

export
addClientCommand : Command -> PServer -> PServer
addClientCommand cmd = record { clientCommands $= append cmd }

export
flushDynamicsCommands : PServer -> PServer
flushDynamicsCommands = record { dynamicsCommands = empty }

export
flushInSessionCommands : PServer -> PServer
flushInSessionCommands = record { serverCommands = empty }

export
flushSessionCommands : PServer -> PServer
flushSessionCommands = record { sessionCommands = empty }

export
flushGameCommands : PServer -> PServer
flushGameCommands = record { gameCommands = empty }

export
flushClientCommands : PServer -> PServer
flushClientCommands = record { clientCommands = empty }

export
flushOutput : PServer -> PServer
flushOutput = flushDynamicsCommands . flushInSessionCommands . flushSessionCommands

export
flushInput : PServer -> PServer
flushInput = flushClientCommands

export
pserverSetBodyData : Objects BodyData -> PServer -> PServer
pserverSetBodyData bodyData' = record { bodyData = bodyData' }

export
pserverGetDimensions : PServer -> Vector2D
pserverGetDimensions = dimensions . mapData

export
getOutside : (dimensions : Vector2D) -> List (ObjectId, BodyData) -> List ObjectId
getOutside (w, h) [] = []
getOutside dimensions@(w, h) ((id, object) :: xs) = let (x, y) = position object in
  if x > w || x < -w || y > h || y < -h
    then id :: getOutside dimensions xs
    else getOutside dimensions xs

module Server.PServer

import Physics.Vector2D

import GameIO
import Objects
import Dynamics.PDynamics
import Commands
import Descriptions.MapDescription
import Exception
import JSONCache
import Timeline

public export
record MapData where
  constructor MkMapData
  mapName : String
  dimensions : Vector2D -- dimensions of current map
  background : Background

mapDescriptionToMapData : MapDescription -> MapData
mapDescriptionToMapData desc = MkMapData (name desc) (dimensions desc) (background desc)

-- these are intended to be sent over the network, dynamics commands are always local
-- which means that a separate client dynamics system will need an additional
-- BodyData parameter in Create
public export
data ServerCommand
  = Create ObjectId ContentReference -- client gets other parameters from dynamics
  | Destroy ObjectId
  | Control Command
  | UpdateNumericProperty ObjectId NumericPropertyId Double

export
Show ServerCommand where
  show (Create id ref) = "create " ++ id ++ " " ++ ref
  show (Control cmd) = "control " ++ show cmd
  show (UpdateNumericProperty object_id prop_id current) = "info update"

public export
LoginResponse : Type
LoginResponse = Checked ObjectId

export
loginFail : (name : String) -> (error : String) -> LoginResponse
loginFail name error = fail $ "couldn't login " ++ name ++ ", error: " ++ error

export -- TODO URGENT unify with Server.createObject
loginSuccess : (id : ObjectId) -> LoginResponse
loginSuccess id = pure id
  -- = pure (id, dynamicsCommands, serverCommands) where
  --     dynamicsCommands : List DynamicsCommand
  --     dynamicsCommands = [createObjectCommand (forCharacter character) character_object id]
  --
  --     serverCommands : List ServerCommand
  --     serverCommands = [Create id (ref character)]

public export
record PServer where
  constructor MkPServer
  idCounter : Nat
  preload : PreloadResults
  mapData : MapData
  loggedIn : Objects Character
  bodyData : Objects BodyData
  dynamicsCommands : List DynamicsCommand -- output
  serverCommands : List ServerCommand -- output
  clientCommands : List Command -- input

export
scounter : PServer -> PServer
scounter = record { idCounter $= S }

export
fromMapPreload : MapDescription -> PreloadResults -> PServer
fromMapPreload desc preload
  = MkPServer 0 preload (mapDescriptionToMapData desc) empty empty empty empty empty

export
addLoggedIn : ObjectId -> Character -> PServer -> PServer
addLoggedIn id character = record { loggedIn $= addObject id character }

export
addDynamicsCommand : DynamicsCommand -> PServer -> PServer
addDynamicsCommand cmd = record { dynamicsCommands $= append cmd }

export
addDynamicsCommands : List DynamicsCommand -> PServer -> PServer
addDynamicsCommands cmds = record { dynamicsCommands $= \xs => xs ++ cmds }

export
addServerCommand : ServerCommand -> PServer -> PServer
addServerCommand cmd = record { serverCommands $= append cmd }

export
addClientCommand : Command -> PServer -> PServer
addClientCommand cmd = record { clientCommands $= append cmd }

export
flushDynamicsCommands : PServer -> PServer
flushDynamicsCommands = record { dynamicsCommands = empty }

export
flushServerCommands : PServer -> PServer
flushServerCommands = record { serverCommands = empty }

export
flushClientCommands : PServer -> PServer
flushClientCommands = record { clientCommands = empty }

export
flushOutput : PServer -> PServer
flushOutput = flushDynamicsCommands . flushServerCommands

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

module Server.PServer

import Physics.Vector2D

import GameIO
import Objects
import Dynamics.PDynamics
import Commands
import Descriptions
import JSONCache

public export
record MapData where
  constructor MkMapData
  mapName : String
  dimensions : Vector2D -- dimensions of current map
  background : Background

mapDescriptionToMapData : MapDescription -> MapData
mapDescriptionToMapData desc = MkMapData (name desc) (dimensions desc) (background desc)

-- these are intended to be sent over the network, dynamics commands are always local
public export
data ServerCommand
  = Create ObjectId ResourceReference -- client gets other parameters from dynamics
  | Control Command
  | InfoUpdate

export
Show ServerCommand where
  show (Create id ref) = "create " ++ id ++ " " ++ ref
  show (Control cmd) = "control " ++ show cmd
  show InfoUpdate = "info update"

public export
record PServer where
  constructor MkPServer
  idCounter : Nat
  preload : PreloadResults
  dynamicsCommands : List DynamicsCommand
  serverCommands : List ServerCommand
  mapData : MapData

export
scounter : PServer -> PServer
scounter = record { idCounter $= S }

export
fromMapPreload : MapDescription -> PreloadResults -> PServer
fromMapPreload desc preload
  = MkPServer 0 preload empty empty (mapDescriptionToMapData desc)

export
addDynamicsCommand : DynamicsCommand -> PServer -> PServer
addDynamicsCommand cmd = record { dynamicsCommands $= (::) cmd }

export
addClientCommand : ServerCommand -> PServer -> PServer
addClientCommand cmd = record { serverCommands $= (::) cmd }

export
flushCommands : PServer -> PServer
flushCommands = record { serverCommands = empty, dynamicsCommands = empty }

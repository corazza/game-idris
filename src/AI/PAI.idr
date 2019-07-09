module AI.PAI

import Data.AVL.Dict

import Common
import Events
import AI.Controller
import Data.AVL.DDict
import Physics.Vector2D
import Objects

public export
Commands : Type
Commands = Dict ObjectId (List Command)

export
removeCommands : Commands -> Commands
removeCommands = map (const empty)

public export
AIControllers : Type
AIControllers = DDict ObjectId AIController

public export
record ObjectInfo where
  constructor MkObjectInfo
  id : ObjectId
  position : Vector2D
%name ObjectInfo objectInfo

export
toInfo : Object -> ObjectInfo
toInfo object = MkObjectInfo (id object) (position object)

public export
ObjectsInfo : Type
ObjectsInfo = DDict ObjectId (Maybe ObjectInfo)

updateObjectsInfo : List ObjectInfo -> ObjectsInfo -> ObjectsInfo
updateObjectsInfo [] dict = dict
updateObjectsInfo (info :: xs) dict
  = updateObjectsInfo xs (insert (id info) (Just info) dict)

public export
record PAI where
  constructor MkPAI
  time : Int
  commands : Commands
  controllers : AIControllers
  objects : ObjectsInfo
%name PAI pai

export
emptyPAI : PAI
emptyPAI = MkPAI 0 empty empty empty

export
addInfo : ObjectId -> PAI -> PAI
addInfo id = record { objects $= insert id Nothing }

export
removeInfo : ObjectId -> PAI -> PAI
removeInfo id = record { objects $= delete id }

export
updateObjectsInfoPAI : List ObjectInfo -> PAI -> PAI
updateObjectsInfoPAI xs = record { objects $= updateObjectsInfo xs }

export -- PERF maybe DDict delete doesn't remove the keys
getRelevantObjects : PAI -> List ObjectId
getRelevantObjects = keys . objects

export
addControllerPAI : AIController -> ObjectId -> PAI -> PAI
addControllerPAI controller id = record { controllers $= insert id controller }

export
removeControllerPAI : ObjectId -> PAI -> PAI
removeControllerPAI id = record { controllers $= DDict.delete id }

export
getControllerPAI : ObjectId -> PAI -> Maybe AIController
getControllerPAI id = lookup id . controllers

export
passedPAI : Int -> PAI -> PAI
passedPAI dt = record { time $= (+ dt) }

export
removeCommandsPAI : PAI -> PAI
removeCommandsPAI = record { commands $= removeCommands }

export
addCommandPAI : ObjectId -> Command -> PAI -> PAI
addCommandPAI id command = record { commands $= update id (command ::) }

export
updateController : ObjectId -> (f : AIController -> AIController) -> PAI -> PAI
updateController id f = record { controllers $= DDict.update id f }

export
emptyCommandsFor : ObjectId -> PAI -> PAI
emptyCommandsFor id = record { commands $= insert id empty }

export
getPosition : ObjectId -> PAI -> Maybe Vector2D
getPosition id pai = map position $ join $ DDict.lookup id $ objects pai

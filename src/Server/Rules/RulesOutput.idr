module Server.Rules.RulesOutput

import Client.ClientCommands
import Descriptions.MapDescription
import Dynamics.DynamicsControl
import Dynamics.MoveDirection
import Commands
import Objects
import GameIO
import Timeline

public export
data RulesOutput
  = Create Creation
  | RunQuery ObjectId String Double
  | ApplyImpulse ObjectId Vector2D
  | SetFacing ObjectId MoveDirection
  | RuleCommand Command
  | Death ObjectId
  | NumericPropertyCurrent ObjectId NumericPropertyId Double
  | ExitTo ObjectId ContentReference
  | UpdateCharacter CharacterId (Character -> Character)
  | RulesClientCommand CharacterId ClientCommand
  | SetMaskBits ObjectId (List String)
  | UnsetMaskBits ObjectId (List String)
  | SetAttackShowing ObjectId ContentReference | UnsetAttackShowing ObjectId

export
Show RulesOutput where
  show (Create creation) = "create " ++ show creation
  show _ = "show unimplemented"

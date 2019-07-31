module Server.Rules.RulesOutput

import Descriptions.MapDescription
import Commands
import Objects
import GameIO
import Timeline

public export
data RulesOutput
  = Create Creation
  | RuleCommand Command
  | Death ObjectId
  | NumericPropertyCurrent ObjectId NumericPropertyId Double
  | ExitTo ObjectId ContentReference
  | UpdateCharacter CharacterId (Character -> Character)

export
Show RulesOutput where
  show (Create creation) = "create " ++ show creation
  show _ = "show unimplemented"

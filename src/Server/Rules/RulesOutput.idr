module Server.Rules.RulesOutput

import Descriptions.MapDescription
import Commands
import Objects
import GameIO

public export
data RulesOutput
  = Create Creation
  | RuleCommand Command
  | Death ObjectId
  | NumericPropertyCurrent ObjectId NumericPropertyId Double
  | ExitTo ObjectId ContentReference

export
Show RulesOutput where
  show (Create creation) = "create " ++ show creation

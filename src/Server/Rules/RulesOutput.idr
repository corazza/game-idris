module Server.Rules.RulesOutput

import Descriptions.MapDescription
import Commands
import Objects

public export
data RulesOutput
  = Create Creation
  | RuleCommand Command
  | Death ObjectId
  | NumericPropertyCurrent ObjectId NumericPropertyId Double

export
Show RulesOutput where
  show (Create creation) = "create " ++ show creation

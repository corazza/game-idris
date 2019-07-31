module Client.UI

import Control.ST
import Control.ST.ImplicitCall

import Client.UI.PUI
import Descriptions.UIDescription
import JSONCache

interface UI (m : Type -> Type) where
  SUI : Type

  startUI : (preload : PreloadResults) ST m Var [add SUI]
  endUI : (ui : Var) -> ST m () [remove ui SUI]

  processEvent : (ui : Var)

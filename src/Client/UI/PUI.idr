module Client.UI.PUI

import GameIO
import Descriptions.UIDescription

data UIEvent
  = Click Int Int
  | MoveMouse Int Int

public export
record PUI where
  constructor MkPUI
  descriptions : Dict ContentReference UIDescription

updateChildren : ContentReference ->
                 SurfaceId ->
                 (f : List SurfaceDescription -> List SurfaceDescription) ->
                 PUI -> PUI

module Client.SDL.Points

public export
ScreenPoint : Type
ScreenPoint = (Int, Int)

export
screenPointsToSDLPoints : List ScreenPoint -> (List Int, List Int)
screenPointsToSDLPoints = unzip

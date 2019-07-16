module Descriptions.AnimationDescription

import GameIO
import Exception

public export
record AnimationDescription where
  constructor MkAnimationDescription
  sheet : ResourceReference
  dimensions : (Int, Int) -- width and height of single sprite in pixels
  nx : Int
  ny : Int
  facingRight : Bool
%name AnimationDescription animation_description

export
Show AnimationDescription where
  show (MkAnimationDescription sheet dimensions nx ny facingRight)
    =  "{ " ++ sheet
    ++ ", " ++ show dimensions
    ++ ", " ++ show nx
    ++ ", " ++ show ny
    ++ ", " ++ show facingRight
    ++ " }"

export
ObjectCaster AnimationDescription where
  objectCast dict = with Checked do
    sheet <- getString "sheet" dict
    dimensions <- getIntPair "dimensions" dict
    nx <- getInt "nx" dict
    let ny = getIntOrDefault "ny" 1 dict
    facingRight <- getBoolOrDefault True "facingRight" dict
    pure $ MkAnimationDescription sheet dimensions nx ny facingRight

module Descriptions.AnimationDescription

import GameIO
import Exception
import Client.SDL

public export
record AnimationDescription where
  constructor MkAnimationDescription
  sheet : ContentReference
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

export
getSrc : (clock : Int) -> (fps : Double) -> AnimationDescription -> SDLRect
getSrc clock fps animation_description
  = let passed_frames = clock `div` cast (1000 / fps)
        frame = passed_frames `mod` (nx animation_description * ny animation_description)
        framex = frame `div` ny animation_description
        framey = frame `div` nx animation_description
        (w, h) = dimensions animation_description
        in MkSDLRect (framex*w) (framey*h) w h

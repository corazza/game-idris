module Rendering

import Control.ST
import Control.ST.ImplicitCall

import Scene.Rendering.PRendering
import Scene.Dynamics
import SDL
import GameIO
import Exception

public export
interface Rendering (m : Type -> Type) where
  SRendering : Type

  startRendering : Int -> Int -> ST m Var [add SRendering]
  endRendering : (rendering : Var) -> ST m () [remove rendering SRendering]

export
GameIO m => Rendering m where
  SRendering = State PRendering

export
render : (Dynamics m, SDL m) =>
         (dynamics : Var) ->
         (sdl : Var) ->
         (prendering : PRendering) ->
         (clock : Int) ->
         ST m () [dynamics ::: SDynamics {m}, sdl ::: SSDL {m}]

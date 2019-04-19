module Main

import Graphics.SDL2 as SDL2
import System as System
import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Physics.Box2D
import Language.JSON

import Draw
import Scene
import Objects
import Events
import Input
import Physics.Vector2D
import Physics.Box2D
import GameIO
import Resources
import Descriptors


GameState : (Monad m, GameIO m, Draw m, ConsoleIO m, Box2DPhysics m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Vector2D,
                           EmptyContext {m}, -- empty context for no-context loaders, ugly af TODO fix
                           (SCache {m} {r=Texture},
                            SCache {m} {r=MapDescriptor},
                            SCache {m} {r=ObjectDescriptor}),
                           State Int]

screenScale : Double
screenScale = 33

resolution : (Int, Int)
resolution = (1280, 960)

Cast (Int, Int) (Double, Double) where
  cast (x, y) = (cast x, cast y)

resolution' : (Double, Double)
resolution' = cast resolution

positionToScreen : (camera : Vector2D) -> (position : Vector2D) -> (Int, Int)
positionToScreen (cx, cy) (ox, oy)
  = let (x, y) = screenScale `scale` (ox - cx, cy - oy) in
        cast (x + (fst resolution')/2, y + (snd resolution')/2)

dimToScreen : (dim : Vector2D) -> (Int, Int)
dimToScreen (x, y) = cast $ (screenScale * x, screenScale * y)

-- drawScene : (Draw m, ConsoleIO m, Box2DPhysics m, Scene m) => (state : Var) ->
--             ST m () [state ::: GameState {m}]
-- drawScene state = (with ST do
--   [draw, scene, camera, lastms] <- split state
--   clear draw
--   drawObjects draw !(read camera) !(getObjects scene)
--   present draw
--   combine state [draw, scene, camera, lastms]) where
--     drawObjects : (Draw m, ConsoleIO m) =>
--                   (draw : Var) -> (camera : Vector2D) -> List Object ->
--                   ST m () [draw ::: SDraw {m}]
--     drawObjects draw camera [] = pure ()
--     drawObjects draw camera (object :: xs) = with ST do
--       let (x, y) = positionToScreen camera ((position object) - (dim object))
--       let (w, h) = dimToScreen (2 `scale` dim object)
--       let dst = MkSDLRect x y w h
--       let deg_angle = (angle object) / (2*pi) * 360.0
--       drawWholeCenter draw (texture object) dst deg_angle
--       drawObjects draw camera xs
--
-- loop : (Monad m,
--         ConsoleIO m,
--         Box2DPhysics m,
--         GameIO m,
--         Scene m,
--         Draw m) =>
--         (state : Var) ->
--         ST m () [state ::: GameState {m}]
-- loop state = with ST do
--   Right events <- poll
--                | pure ()
--   [draw, scene, camera, lastms] <- split state
--   controlEvent scene "player" (case events of
--                                     [] => Nothing
--                                     (x :: xs) => Just x)
--   beforems <- ticks
--   iterate scene (beforems - !(read lastms))
--   write lastms beforems
--   combine state [draw, scene, camera, lastms]
--   drawScene state
--   loop state


-- TODO all resources are loaded w/ IO, use that information


game : (Monad m, ConsoleIO m, Draw m, GameIO m, Box2DPhysics m, Scene m) => ST m () []
game {m} = with ST do
  draw <- initDraw (fst resolution) (snd resolution)
  textureCache <- initCache {r=Texture}
  mapCache <- initCache {r=MapDescriptor}
  emptyContext <- createEmptyContext


  Just map <- get {m} {r=MapDescriptor} mapCache emptyContext "likert"
           | Nothing => ?oops1


  Just playerTexture <- get {m} {r=Texture} textureCache draw "disciple"
                     | Nothing => ?oops2


  scene <- startScene map

  quitDraw draw
  endScene scene
  quitCache {r=Texture} textureCache
  quitCache {r=MapDescriptor} mapCache
  deleteEmptyContext emptyContext  


  -- let playerBoxDesc = MkBoxDescription 5 Dynamic (0.5, 48.0/33.0/2.0)
  -- let player = MkObject "player" (11, 20) (pi/4 + 0.1) playerBoxDesc playerTexture noControl
  -- addObject scene player
  -- state <- new ()
  -- camera <- new (0, 0)
  -- lastms <- new !ticks
  -- combine state [draw, scene, camera, lastms]
  -- loop state
  -- [draw, scene, camera, lastms] <- split state
  -- quitDraw draw
  -- endScene scene
  -- delete camera; delete lastms
  -- delete state

main : IO ()
main = run game
-- main = print !test

module Main

import Graphics.SDL2 as SDL2
import System as System
import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict

import Draw
import Col
import Scene
import Objects
import Events

GameState : Draw m => Type
GameState {m} = Composite [SDraw {m}, SScene]

drawScene : Draw m => (state : Var) ->
            ST m () [state ::: GameState {m}]
drawScene state = (with ST do
  [draw, scene] <- split state
  [idCounter, objects, events] <- split scene
  clear draw
  objectDict <- read objects
  combine scene [idCounter, objects, events]
  drawObjects draw (toList objectDict)
  present draw
  combine state [draw, scene]) where
    drawObjects : Draw m => (draw : Var) -> List Object -> ST m () [draw ::: SDraw {m}]
    drawObjects draw [] = pure ()
    drawObjects draw (object :: xs) = with ST do
      let dst = MkSDLRect (x object) (y object) (w object) (h object)
      drawTexture draw (texture object) Nothing (Just dst)
      drawObjects draw xs


loop : (ConsoleIO m, Draw m) => (state : Var) ->
       ST m () [state ::: GameState {m}]
loop state = with ST do
  event <- call poll

  -- filledRectangle draw (100, 100) (50, 50) (MkCol 255 0 0 128)
  -- filledEllipse draw (10, 10) (20, 20) (MkCol 0 255 0 128)

  [draw, scene] <- split state

  iterate scene

  combine state [draw, scene]

  drawScene state

  case event of
      Nothing => loop state
      Just AppQuit => pure ()
      Just _ => loop state

game : (ConsoleIO m, Draw m) => ST m () []
game = with ST do
  draw <- initDraw 640 480
  scene <- startScene 640 480

  playerTexture <- getTexture draw "disciple"
  let player = MkObject "player" 50 50 0 0 33 48 playerTexture
  addObject scene player

  state <- new ()
  combine state [draw, scene]
  loop state

  [draw, scene] <- split state
  quitDraw draw
  endScene scene
  delete state

main : IO ()
main = run game



-- main = (do
--   renderer <- SDL2.init 640 480
--   renderPresent renderer
--
--   let imageCache = emptyImageCache
--
--   (disciple, imageCache) <- getImage renderer imageCache "disciple"
--
--   eventLoop renderer initState disciple)
--     where
--       eventLoop : Renderer -> GameState -> Image -> IO ()
--       processEvent : Renderer -> GameState -> Image -> Maybe Event -> IO ()
--
--       eventLoop renderer state image = do
--         event <- pollEvent
--
--         True <- SDL2.setRendererDrawColor renderer 0 0 0 0
--           | abort "setRendererDrawColor"
--
--         SDL2.renderClear renderer
--
--         filledRect renderer 100 100 50 50 255 0 0 128
--         filledEllipse renderer (x state) (y state) 20 20 0 255 0 128
--         when (((f state) `mod` 100) == 0) $ print (f state)
--
--         rc <- SDL2.renderCopy' renderer (texture image) Nothing (Just (dst state))
--
--         when (rc /= 0) $ abort "renderCopy"
--
--         SDL2.renderPresent renderer -- update screen
--
--         processEvent renderer (record { dst $= translate (mx state) (my state) } state) image event
--
--         -- processEvent renderer (record {x $= (+ mx state), y $= (+ my state)} state) image event
--
--       processEvent r state image (Just (KeyDown KeyLeftArrow)) =
--         eventLoop r (record {mx = -1} state) image
--       processEvent r state image (Just (KeyUp KeyLeftArrow)) =
--         eventLoop r (record {mx = 0} state) image
--       processEvent r state image (Just (KeyDown KeyRightArrow)) =
--         eventLoop r (record {mx = 1} state) image
--       processEvent r state image (Just (KeyUp KeyRightArrow)) =
--         eventLoop r (record {mx = 0} state) image
--       processEvent r state image (Just (KeyDown KeyUpArrow)) =
--         eventLoop r (record {my = -1} state) image
--       processEvent r state image (Just (KeyUp KeyUpArrow)) =
--         eventLoop r (record {my = 0} state) image
--       processEvent r state image (Just (KeyDown KeyDownArrow)) =
--         eventLoop r (record {my = 1} state) image
--       processEvent r state image (Just (KeyUp KeyDownArrow)) =
--         eventLoop r (record {my = 0} state) image
--       processEvent r state image (Just AppQuit) = pure ()
--       processEvent r state image (Just (KeyDown (KeyAny k))) = do
--         print k
--         eventLoop r state image
--       processEvent r state image (Just (MouseMotion mousex mousey _ _)) = do
--         print (mousex, mousey)
--         eventLoop r state image
--       processEvent r state image (Just (MouseButtonUp Left mousex mousey)) = do
--         print (mousex, mousey)
--         eventLoop r (record {x=mousex, y=mousey} state) image
--       processEvent r state image _ = eventLoop r state image

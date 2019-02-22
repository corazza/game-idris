module Main

import Graphics.SDL2 as SDL2
import System as System

import Resources

record GameState where
  constructor MkGameState
  f : Int
  dst : SDLRect
  x, y : Int
  mx, my : Int

initState : GameState
initState = MkGameState 0 (MkSDLRect 20 20 40 40) 320 200 0 0

abort : (msg: String) -> IO ()
abort msg = do
  err <- getError
  fPutStr stderr $ msg ++ " failed:" ++ err
  fflush stderr
  System.exit 1

main : IO ()
main = (do
  renderer <- SDL2.init 640 480
  renderPresent renderer

  let imageCache = emptyImageCache

  (disciple, imageCache) <- getImage renderer imageCache "disciple"

  eventLoop renderer initState disciple)
    where
      eventLoop : Renderer -> GameState -> Image -> IO ()
      processEvent : Renderer -> GameState -> Image -> Maybe Event -> IO ()

      eventLoop renderer state image = do
        event <- pollEvent

        True <- SDL2.setRendererDrawColor renderer 0 0 0 0
          | abort "setRendererDrawColor"

        SDL2.renderClear renderer

        filledRect renderer 100 100 50 50 255 0 0 128
        filledEllipse renderer (x state) (y state) 20 20 0 255 0 128
        when (((f state) `mod` 100) == 0) $ print (f state)

        rc <- SDL2.renderCopy' renderer (texture image) Nothing (Just (dst state))

        when (rc /= 0) $ abort "renderCopy"

        SDL2.renderPresent renderer -- update screen

        processEvent renderer (record { dst $= translate (mx state) (my state) } state) image event

        -- processEvent renderer (record {x $= (+ mx state), y $= (+ my state)} state) image event

      processEvent r state image (Just (KeyDown KeyLeftArrow)) =
        eventLoop r (record {mx = -1} state) image
      processEvent r state image (Just (KeyUp KeyLeftArrow)) =
        eventLoop r (record {mx = 0} state) image
      processEvent r state image (Just (KeyDown KeyRightArrow)) =
        eventLoop r (record {mx = 1} state) image
      processEvent r state image (Just (KeyUp KeyRightArrow)) =
        eventLoop r (record {mx = 0} state) image
      processEvent r state image (Just (KeyDown KeyUpArrow)) =
        eventLoop r (record {my = -1} state) image
      processEvent r state image (Just (KeyUp KeyUpArrow)) =
        eventLoop r (record {my = 0} state) image
      processEvent r state image (Just (KeyDown KeyDownArrow)) =
        eventLoop r (record {my = 1} state) image
      processEvent r state image (Just (KeyUp KeyDownArrow)) =
        eventLoop r (record {my = 0} state) image
      processEvent r state image (Just AppQuit) = pure ()
      processEvent r state image (Just (KeyDown (KeyAny k))) = do
        print k
        eventLoop r state image
      processEvent r state image (Just (MouseMotion mousex mousey _ _)) = do
        print (mousex, mousey)
        eventLoop r state image
      processEvent r state image (Just (MouseButtonUp Left mousex mousey)) = do
        print (mousex, mousey)
        eventLoop r (record {x=mousex, y=mousey} state) image
      processEvent r state image _ = eventLoop r state image

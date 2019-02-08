module Main

import Graphics.SDL2

import Resources

record GameState where
  constructor MkGameState
  x, y : Int

main : IO ()
main = (do
  renderer <- SDL2.init 640 480
  renderPresent renderer
  let imageCache = emptyImageCache

  (disciple, cache) <- getImage renderer imageCache "disciple"

  eventLoop renderer (texture disciple) 0 320 200 0 0)
    where
      mutual
        eventLoop : Renderer -> Texture -> Integer -> Int -> Int -> Int -> Int -> IO ()
        eventLoop r t f x y mx my = do
          event <- pollEvent
          filledRect r 0 0 640 480 0 0 0 128
          filledRect r 100 100 50 50 255 0 0 128
          filledEllipse r x y 20 20 0 255 0 128
          when ((f `mod` 100) == 0) $ print f

          SDL2.renderClear r
          rc <- SDL2.renderCopy r t -- draw texture
          --when (rc /= 0) $ abort "renderCopy"
          SDL2.renderPresent r -- update screen

          processEvent r t (f+1) (x+mx) (y+my) mx my event

        processEvent : Renderer -> Texture -> Integer -> Int -> Int -> Int -> Int -> Maybe Event -> IO ()
        processEvent r t f x y mx my _ = eventLoop r t f x y mx my

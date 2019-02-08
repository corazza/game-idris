module Main

import Graphics.SDL2

import Resources

main : IO ()
main = (do
  renderer <- SDL2.init 640 480
  renderPresent renderer
  eventLoop renderer 0 320 200 0 0)
    where
      eventLoop : Renderer -> Integer -> Int -> Int -> Int -> Int -> IO ()
      processEvent : Renderer -> Integer -> Int -> Int -> Int -> Int -> Maybe Event -> IO ()

      eventLoop r f x y mx my = do
        event <- pollEvent
        filledRect r 0 0 640 480 0 0 0 128
        filledRect r 100 100 50 50 255 0 0 128
        filledEllipse r x y 20 20 0 255 0 128
        when ((f `mod` 100) == 0) $ print f
        renderPresent r
        processEvent r (f+1) (x+mx) (y+my) mx my event

      processEvent r f x y mx my (Just (KeyDown KeyLeftArrow)) =
        eventLoop r f x y (-1) my
      processEvent r f x y mx my (Just (KeyUp KeyLeftArrow)) =
        eventLoop r f x y 0 my
      processEvent r f x y mx my (Just (KeyDown KeyRightArrow)) =
        eventLoop r f x y 1 my
      processEvent r f x y mx my (Just (KeyUp KeyRightArrow)) =
        eventLoop r f x y 0 my
      processEvent r f x y mx my (Just (KeyDown KeyUpArrow)) =
        eventLoop r f x y mx (-1)
      processEvent r f x y mx my (Just (KeyUp KeyUpArrow)) =
        eventLoop r f x y mx 0
      processEvent r f x y mx my (Just (KeyDown KeyDownArrow)) =
        eventLoop r f x y mx 1
      processEvent r f x y mx my (Just (KeyUp KeyDownArrow)) =
        eventLoop r f x y mx 0
      processEvent r f x y mx my (Just AppQuit) = pure ()
      processEvent r f x y mx my (Just (KeyDown (KeyAny k))) = do
        print k
        eventLoop r f x y mx my
      processEvent r f x y mx my (Just (MouseMotion mousex mousey _ _)) = do
        print (mousex, mousey)
        eventLoop r f x y mx my
      processEvent r f x y mx my (Just (MouseButtonUp Left mousex mousey)) = do
        print (mousex, mousey)
        eventLoop r f mousex mousey mx my
      processEvent r f x y mx my _ = eventLoop r f x y mx my

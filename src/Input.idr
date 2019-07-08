module Input

import Graphics.SDL2 as SDL2

import Physics.Vector2D
import Camera
import Events

export
keyToCommand : Key -> Maybe Action
keyToCommand KeyUpArrow = Just (Movement Up)
keyToCommand KeyDownArrow = Just (Movement Down)
keyToCommand KeyLeftArrow = Just (Movement Left)
keyToCommand KeyRightArrow = Just (Movement Right)
keyToCommand KeyEsc = Nothing
keyToCommand KeySpace = Just (Movement Up)
keyToCommand KeyTab = Nothing
keyToCommand KeyF1 = Nothing
keyToCommand KeyF2 = Nothing
keyToCommand KeyF3 = Nothing
keyToCommand KeyF4 = Nothing
keyToCommand KeyF5 = Nothing
keyToCommand KeyF6 = Nothing
keyToCommand KeyF7 = Nothing
keyToCommand KeyF8 = Nothing
keyToCommand KeyF9 = Nothing
keyToCommand KeyF10 = Nothing
keyToCommand KeyF11 = Nothing
keyToCommand KeyF12 = Nothing
keyToCommand KeyF13 = Nothing
keyToCommand KeyF14 = Nothing
keyToCommand KeyF15 = Nothing
keyToCommand KeyLShift = Nothing
keyToCommand KeyRShift = Nothing
keyToCommand KeyLCtrl = Nothing
keyToCommand KeyRCtrl = Nothing
keyToCommand (KeyAny x)
  = case x of
         'w' => Just (Movement Up)
         'a' => Just (Movement Left)
         's' => Just (Movement Down)
         'd' => Just (Movement Right)
         _ => Nothing

export
processEvents : Camera -> List SDL2.Event -> Either () (List Command)
processEvents camera xs = processEvents' [] xs where
  mutual
    -- both keypresses and mouse button events generate commands, but SDL2 puts
    -- them on different levels of its event hierarchy. this utility function
    -- brings them on the same level in the game input command hierarchy
    processKey : (acc : List Command) ->
                 (ys' : List SDL2.Event) ->
                 (key : Key) ->
                 (cstr : Action -> Command) ->
                 Either () (List Command)
    processKey acc ys' key cstr = case keyToCommand key of
      Nothing => processEvents' acc ys'
      Just cmd => processEvents' ((cstr cmd)::acc) ys'

    processEvents' : (acc : List Command) -> (xs' : List SDL2.Event) -> Either () (List Command)
    processEvents' acc [] = Right acc
    processEvents' acc (sdlEvent :: ys) = case sdlEvent of
      KeyDown key => processKey acc ys key Start
      KeyUp key => processKey acc ys key Stop
      MouseMotion x y z w => processEvents' acc ys
      MouseButtonDown button x y => let scenePos = screenToPosition camera (x, y) in
        processEvents' (Start (Attack scenePos) :: acc) ys
      MouseButtonUp button x y => let scenePos = screenToPosition camera (x, y) in
        processEvents' (Stop (Attack scenePos) :: acc) ys
      Resize x y => processEvents' acc ys
      AppQuit => Left ()

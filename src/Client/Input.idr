module Client.Input

import Physics.Vector2D

import Client.Rendering.Camera
import Client.SDL
import Commands
import Objects

export
keyToAction : Key -> Maybe Action
keyToAction KeyUpArrow = Just (Movement Up)
keyToAction KeyDownArrow = Just (Movement Down)
keyToAction KeyLeftArrow = Just (Movement Left)
keyToAction KeyRightArrow = Just (Movement Right)
keyToAction KeyEsc = Nothing
keyToAction KeySpace = Just (Movement Up)
keyToAction KeyTab = Nothing
keyToAction KeyF1 = Nothing
keyToAction KeyF2 = Nothing
keyToAction KeyF3 = Nothing
keyToAction KeyF4 = Nothing
keyToAction KeyF5 = Nothing
keyToAction KeyF6 = Nothing
keyToAction KeyF7 = Nothing
keyToAction KeyF8 = Nothing
keyToAction KeyF9 = Nothing
keyToAction KeyF10 = Nothing
keyToAction KeyF11 = Nothing
keyToAction KeyF12 = Nothing
keyToAction KeyF13 = Nothing
keyToAction KeyF14 = Nothing
keyToAction KeyF15 = Nothing
keyToAction KeyLShift = Just Walk
keyToAction KeyRShift = Nothing
keyToAction KeyLCtrl = Nothing
keyToAction KeyRCtrl = Nothing
keyToAction (KeyAny x)
  = case x of
         'w' => Just (Movement Up)
         'a' => Just (Movement Left)
         's' => Just (Movement Down)
         'd' => Just (Movement Right)
         _ => Nothing

export
processEvents : Camera -> List SDL2.Event -> Either () (List (ObjectId -> Command))
processEvents camera xs = processEvents' [] xs where
  mutual
    -- both keypresses and mouse button events generate commands, but SDL2 puts
    -- them on different levels of its event hierarchy. this utility function
    -- brings them on the same level in the game input command hierarchy
    processKey : (acc : List (ObjectId -> Command)) ->
                 (ys' : List SDL2.Event) ->
                 (key : Key) ->
                 (cstr : Action -> ObjectId -> Command) ->
                 Either () (List (ObjectId -> Command))
    processKey acc ys' key cstr = case keyToAction key of
      Nothing => processEvents' acc ys'
      Just cmd => processEvents' ((cstr cmd)::acc) ys'

    processEvents' : (acc : List (ObjectId -> Command)) -> (xs' : List SDL2.Event) -> Either () (List (ObjectId -> Command))
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

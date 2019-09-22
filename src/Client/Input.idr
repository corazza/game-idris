module Client.Input

import Physics.Vector2D
import Graphics.SDL2

import Client.Rendering.Camera
import Client.ClientCommands
import Commands
import Objects

export
processKey : Key -> Maybe (Either ClientAction Action)
processKey KeyUpArrow = Just $ Right $ Movement Up
processKey KeyDownArrow = Just $ Right $ Movement Down
processKey KeyLeftArrow = Just $ Right $ Movement Left
processKey KeyRightArrow = Just $ Right $ Movement Right
processKey KeyEsc = Just $ Left MainMenu
processKey KeySpace = Just $ Right $ Movement Up
processKey KeyTab = Nothing
processKey KeyF1 = Nothing
processKey KeyF2 = Nothing
processKey KeyF3 = Nothing
processKey KeyF4 = Nothing
processKey KeyF5 = Nothing
processKey KeyF6 = Nothing
processKey KeyF7 = Nothing
processKey KeyF8 = Nothing
processKey KeyF9 = Nothing
processKey KeyF10 = Nothing
processKey KeyF11 = Nothing
processKey KeyF12 = Nothing
processKey KeyF13 = Nothing
processKey KeyF14 = Nothing
processKey KeyF15 = Nothing
processKey KeyLShift = Just $ Right Walk
processKey KeyRShift = Nothing
processKey KeyLCtrl = Nothing
processKey KeyRCtrl = Nothing
processKey (KeyAny x)
  = case x of
         'w' => Just $ Right $ Movement Up
         'a' => Just $ Right $ Movement Left
         's' => Just $ Right $ Movement Down
         'd' => Just $ Right $ Movement Right
         'e' => Just $ Right $ Interact 2
         'i' => Just $ Left Inventory
         _ => Nothing

startStopOrClient : (cstr_left : ClientAction -> ClientCommand) ->
                    (cstr_right : Action -> Command) ->
                    (key : Key) ->
                    Maybe (Either ClientCommand Command)
startStopOrClient cstr_left cstr_right key = case processKey key of -- TODO concise
                                Nothing => Nothing
                                Just (Left clientAction) => Just $ Left $ cstr_left clientAction
                                Just (Right action) => Just $ Right $ cstr_right action

processEvent : ObjectId -> Camera -> SDL2.Event -> Either () (Maybe (Either ClientCommand Command))
processEvent id camera (KeyDown x) = Right $ startStopOrClient Start (flip Start id) x
processEvent id camera (KeyUp x) = Right $ startStopOrClient Stop (flip Stop id) x
processEvent id camera (MouseMotion x y z w)
  = Right $ Just $ Left $ Mouse $ Move x y
processEvent id camera (MouseButtonDown button x y)
  = Right $ Just $ Left $ Mouse $ ButtonDown x y
processEvent id camera (MouseButtonUp button x y)
  = Right $ Just $ Left $ Mouse $ ButtonUp x y
processEvent id camera (Scroll x y) = Right $ Just $ Left $ Stop $ Zoom y
processEvent id camera (Resize x y) = Right Nothing
processEvent id camera AppQuit = Left ()

export
processEvents : ObjectId ->
                Camera ->
                List SDL2.Event ->
                Either () (List ClientCommand, List Command)
processEvents id camera xs = processEvents' xs ([], []) where
  processEvents' : List SDL2.Event ->
                   (List ClientCommand, List Command) ->
                   Either () (List ClientCommand, List Command)
  processEvents' [] acc = Right acc
  processEvents' (sdl_event::sdl_events) acc@(clientCommands, commands)
    = case processEvent id camera sdl_event of
      Left _ => Left ()
      Right Nothing => processEvents' sdl_events acc
      Right (Just (Left clientCommand)) =>
        processEvents' sdl_events (clientCommand::clientCommands, commands)
      Right (Just (Right command)) =>
        processEvents' sdl_events (clientCommands, command::commands)

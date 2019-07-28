module Client.Input

import Physics.Vector2D

import Client.Rendering.Camera
import Client.SDL
import Client.ClientCommands
import Commands
import Objects

export
processKey : Key -> Maybe (Either ClientCommand Action)
processKey KeyUpArrow = Just $ Right $ Movement Up
processKey KeyDownArrow = Just $ Right $ Movement Down
processKey KeyLeftArrow = Just $ Right $ Movement Left
processKey KeyRightArrow = Just $ Right $ Movement Right
processKey KeyEsc = Nothing
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
         _ => Nothing

startStopOrClient : (cstr : Action -> Command) -> Key -> Maybe (Either ClientCommand Command)
startStopOrClient cstr x = case processKey x of -- TODO concise
                                Nothing => Nothing
                                Just (Left clientCommand) => Just $ Left clientCommand
                                Just (Right action) => Just $ Right $ cstr action

processEvent : ObjectId -> Camera -> SDL2.Event -> Either () (Maybe (Either ClientCommand Command))
processEvent id camera (KeyDown x) = Right $ startStopOrClient (flip Start id) x
processEvent id camera (KeyUp x) = Right $ startStopOrClient (flip Stop id) x
processEvent id camera (MouseMotion x y z w) = Right Nothing
processEvent id camera (MouseButtonDown button x y)
  = Right $ Just $ Right $ Start (Attack $ screenToPosition camera (x, y)) id
processEvent id camera (MouseButtonUp button x y)
  = Right $ Just $ Right $ Stop (Attack $ screenToPosition camera (x, y)) id
processEvent id camera (Scroll x y) = Right $ Just $ Left $ Zoom y
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

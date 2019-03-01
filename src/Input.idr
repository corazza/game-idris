module Input

import Graphics.SDL2 as SDL2

%access public export

data Direction = Left | Right | Up | Down

data Command = Movement Direction
             | Attack

data InputEvent = CommandStart Command
                | CommandStop Command

keyToCommand : Key -> Maybe Command
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

processEvent : Maybe SDL2.Event -> Either () (Maybe InputEvent)
processEvent Nothing = Right Nothing
processEvent (Just AppQuit) = Left ()
processEvent (Just (KeyDown key)) = Right $ CommandStart <$> keyToCommand key
processEvent (Just (KeyUp key)) = Right $ CommandStop <$> keyToCommand key
processEvent (Just (MouseMotion x y z w)) = Right Nothing
processEvent (Just (MouseButtonDown x y z)) = Right Nothing
processEvent (Just (MouseButtonUp x y z)) = Right Nothing
processEvent (Just (Resize x y)) = Right Nothing

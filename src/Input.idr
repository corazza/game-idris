module Input

import Graphics.SDL2 as SDL2

%access public export

data Direction = Left | Right | Up | Down

data Command = Movement Direction
             | Attack Int Int -- x, y of screen

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

-- TODO useless?
||| Left () for quit, Right (Maybe InputEvent) for event
-- processEvent : Maybe SDL2.Event -> Either () (Maybe InputEvent)
-- processEvent Nothing = Right Nothing
-- processEvent (Just AppQuit) = Left ()
-- processEvent (Just (KeyDown key)) = Right $ CommandStart <$> keyToCommand key
-- processEvent (Just (KeyUp key)) = Right $ CommandStop <$> keyToCommand key
-- processEvent (Just (MouseMotion x y z w)) = Right Nothing
-- processEvent (Just (MouseButtonDown x y z)) = Right $ CommandS
-- processEvent (Just (MouseButtonUp x y z)) = Right Nothing
-- processEvent (Just (Resize x y)) = Right Nothing

processEvents : List SDL2.Event -> Either () (List InputEvent)
processEvents xs = processEvents' [] xs where
  mutual
    -- both keypresses and mouse button events generate commands, but SDL2 puts
    -- them on different levels of its event hierarchy. this utility function
    -- brings them on the same level in the game input command hierarchy
    processKey : (acc : List InputEvent) ->
                     (ys' : List SDL2.Event) ->
                     (key : Key) ->
                     (cstr : Command -> InputEvent) ->
                     Either () (List InputEvent)
    processKey acc ys' key cstr = case keyToCommand key of
      Nothing => processEvents' acc ys'
      Just cmd => processEvents' ((cstr cmd)::acc) ys'

    processEvents' : (acc : List InputEvent) -> (xs' : List SDL2.Event) -> Either () (List InputEvent)
    processEvents' acc [] = Right acc
    processEvents' acc (sdlEvent :: ys) = case sdlEvent of
      KeyDown key => processKey acc ys key CommandStart
      KeyUp key => processKey acc ys key CommandStop
      MouseMotion x y z w => processEvents' acc ys
      MouseButtonDown button x y => processEvents' (CommandStop (Attack x y) :: acc) ys
      MouseButtonUp button x y => processEvents' (CommandStop (Attack x y) :: acc) ys
      Resize x y => processEvents' acc ys
      AppQuit => Left ()

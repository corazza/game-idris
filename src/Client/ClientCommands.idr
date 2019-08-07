module Client.ClientCommands

public export
data ClientAction = MainMenu | Zoom Int

export
Show ClientAction where
  show MainMenu = "main menu"
  show (Zoom x) = "zoom " ++ show x

public export
data ClientCommand
  = Start ClientAction
  | Stop ClientAction

export
Show ClientCommand where
  show (Start action) = "start " ++ show action
  show (Stop action) = "stop " ++ show action

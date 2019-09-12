module Client.ClientCommands

public export
data ClientAction = MainMenu | Inventory | Zoom Int

export
Show ClientAction where
  show MainMenu = "main menu"
  show Inventory = "inventory"
  show (Zoom x) = "zoom " ++ show x

public export
data MouseEvent
  = ButtonUp Int Int
  | ButtonDown Int Int
  | Move Int Int

export
Show MouseEvent where
  show (ButtonUp x y) = "button up (" ++ show x ++ ", " ++ show y ++ ")"
  show (ButtonDown x y) = "button down (" ++ show x ++ ", " ++ show y ++ ")"
  show (Move x y) = "move (" ++ show x ++ ", " ++ show y ++ ")"

public export
data ClientCommand
  = Start ClientAction
  | Stop ClientAction
  | Mouse MouseEvent
  | RefreshInventory

export
Show ClientCommand where
  show (Start action) = "start " ++ show action
  show (Stop action) = "stop " ++ show action
  show (Mouse event) = show event
  show RefreshInventory = "refresh inventory"

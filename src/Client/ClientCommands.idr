module Client.ClientCommands

public export
data ClientCommand
  = Zoom Int

export
Show ClientCommand where
  show (Zoom x) = "zoom " ++ show x

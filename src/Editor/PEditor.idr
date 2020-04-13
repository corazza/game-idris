module Editor.PEditor

import JSONCache
import GameIO

public export
record PEditor where
  constructor MkPEditor
  preload : PreloadResults

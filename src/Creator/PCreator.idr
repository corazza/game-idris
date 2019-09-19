module Creator.PCreator

import JSONCache
import GameIO

public export
record PCreator where
  constructor MkPCreator
  preload : PreloadResults

module Client.SDL.PSDL

import JSONCache

public export
record PSDL where
  constructor MkPSDL
  preload : PreloadResults

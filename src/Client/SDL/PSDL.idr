module Client.SDL.PSDL

import Graphics.SDL2 as SDL2
import Data.AVL.Dict

import GameIO
import JSONCache

public export
WavCache : Type
WavCache = Dict String Wav

public export
MusicCache : Type
MusicCache = Dict String Music

export
loadWav : (filepath : String) -> IO (Checked Wav)
loadWav filepath = SDL2.loadWav filepath >>= pure . pure

export
loadMusic : (filepath : String) -> IO (Checked Music)
loadMusic filepath = SDL2.loadMusic filepath >>= pure . pure

public export
record PSDL where
  constructor MkPSDL
  preload : PreloadResults
  wavCache : WavCache
  musicCache : MusicCache

export
fromPreload : PreloadResults -> PSDL
fromPreload preload = MkPSDL preload empty empty

export
psdlAddWav : (ref : ContentReference) -> Wav -> PSDL -> PSDL
psdlAddWav ref wav = record { wavCache $= insert ref wav }

export
psdlAddMusic : (ref : ContentReference) -> Music -> PSDL -> PSDL
psdlAddMusic ref music = record { musicCache $= insert ref music }

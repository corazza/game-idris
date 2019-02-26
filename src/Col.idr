%access public export

data Col = MkCol Int Int Int Int

black : Col
black = MkCol 0 0 0 255

red : Col
red = MkCol 255 0 0 255

green : Col
green = MkCol 0 255 0 255

blue : Col
blue = MkCol 0 0 255 255

cyan : Col
cyan = MkCol 0 255 255 255

magenta : Col
magenta = MkCol 255 0 255 255

yellow : Col
yellow = MkCol 255 255 0 255

white : Col
white = MkCol 255 255 255 255

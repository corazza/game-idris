import Control.ST
import Graphics.SDL2 as SDL2

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

interface Draw (m : Type -> Type) where
  SRenderer : Type

  init : Int -> Int -> ST m Var [add SRenderer]
  quit : (srenderer : Var) -> ST m () [remove srenderer SRenderer]

  poll : ST m (Maybe Event) []

  clear : (renderer : Var) -> ST m Int [renderer ::: SRenderer]
  present : (renderer : Var) -> ST m () [renderer ::: SRenderer]

  filledRectangle : (win : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [win ::: SRenderer]
  drawLine : (win : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [win ::: SRenderer]

  drawTexture : (renderer : Var) ->
                (texture : Texture) ->
                Maybe SDLRect ->
                Maybe SDLRect ->
                ST m Int [renderer ::: SRenderer]

implementation Draw IO where
  SRenderer = State Renderer

  init x y = do renderer <- lift $ init x y
                var <- new renderer
                pure var

  quit win = do lift quit
                delete win

  poll = lift pollEvent

  clear srenderer = do renderer <- read srenderer
                       True <- lift $ SDL2.setRendererDrawColor renderer 0 0 0 0
                            | pure (-1)
                       lift $ SDL2.renderClear renderer

  present srenderer = do renderer <- read srenderer
                         lift $ SDL2.renderPresent renderer

  filledRectangle win (x, y) (ex, ey) (MkCol r g b a)
       = do srf <- read win
            lift $ filledRect srf x y ex ey r g b a

  drawLine win (x, y) (ex, ey) (MkCol r g b a)
       = do srf <- read win
            lift $ drawLine srf x y ex ey r g b a

  drawTexture srenderer texture src dst
    = do renderer <- read srenderer
         lift $ SDL2.renderCopy' renderer texture src dst

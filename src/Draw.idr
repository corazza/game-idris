import Data.SortedMap
import Control.ST
import Graphics.SDL2 as SDL2

import Events
import Scene
import Objects
import Input

import Col

%access public export

ImageCache : Type
ImageCache = SortedMap String Texture

emptyImageCache : ImageCache
emptyImageCache = empty

interface Draw (m : Type -> Type) where
  SDraw : Type

  initDraw : Int -> Int -> ST m Var [add SDraw]
  quitDraw : (draw : Var) -> ST m () [remove draw SDraw]

  poll : ST m (Either () (Maybe InputEvent)) []

  clear : (draw : Var) -> ST m Int [draw ::: SDraw]
  present : (draw : Var) -> ST m () [draw ::: SDraw]

  filledRectangle : (draw : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [draw ::: SDraw]
  filledEllipse : (draw : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [draw ::: SDraw]
  drawLine : (draw : Var) -> (Int, Int) -> (Int, Int) -> Col -> ST m () [draw ::: SDraw]

  getTexture : (draw : Var) -> (name : String) -> ST m Texture [draw ::: SDraw]

  drawTexture : (draw : Var) ->
                (texture : Texture) ->
                (src : Maybe SDLRect) ->
                (dst : Maybe SDLRect) ->
                ST m Int [draw ::: SDraw]

implementation Draw IO where
  SDraw = Composite [State Renderer, State ImageCache]

  initDraw x y = with ST do
              renderer <- new $ !(lift (init x y))
              imageCache <- new emptyImageCache
              draw <- new ()
              combine draw [renderer, imageCache]
              pure draw

  -- TODO: free images?
  quitDraw draw = with ST do
                [renderer, imageCache] <- split draw
                lift quit
                delete renderer; delete imageCache; delete draw

  -- poll = with ST do
  --   event <- lift pollEvent
  --   pure (processEvent event)

  -- poll = (lift pollEvent) >>= (pure . processEvent)
  poll = lift pollEvent >>= (\event => pure (processEvent event))

  clear draw = with ST do
                [srenderer, imageCache] <- split draw
                renderer <- read srenderer
                lift $ SDL2.setRendererDrawColor renderer 0 0 0 0
                lift $ SDL2.renderClear renderer
                combine draw [srenderer, imageCache]
                pure 1 -- TODO: should process SDL2 errors

  present draw = with ST do
                  [renderer, imageCache] <- split draw
                  lift $ SDL2.renderPresent !(read renderer)
                  combine draw [renderer, imageCache]

  filledRectangle draw (x, y) (ex, ey) (MkCol r g b a)
    = with ST do [renderer, imageCache] <- split draw
                 lift $ filledRect !(read renderer) x y ex ey r g b a
                 combine draw [renderer, imageCache]

  filledEllipse draw (x, y) (rx, ry) (MkCol r g b a)
    = with ST do [renderer, imageCache] <- split draw
                 lift $ filledEllipse !(read renderer) x y rx ry r g b a
                 combine draw [renderer, imageCache]

  drawLine draw (x, y) (ex, ey) (MkCol r g b a)
    = with ST do [renderer, imageCache] <- split draw
                 lift $ drawLine !(read renderer) x y ex ey r g b a
                 combine draw [renderer, imageCache]

  getTexture draw name
    = with ST do [renderer, imageCache] <- split draw
                 (texture, cache) <- lift $
                      cachedLoad !(read renderer) !(read imageCache) name
                 write imageCache cache
                 combine draw [renderer, imageCache]
                 pure texture
       where
         nameToFilepath : String -> String
         nameToFilepath s = "res/images/" ++ s ++ ".bmp"

         -- TODO: handle errors
         loadTexture : Renderer -> (filepath : String) -> IO Texture
         loadTexture renderer filepath = do
           bmp <- SDL2.loadBMP  filepath
           texture <- SDL2.createTextureFromSurface renderer bmp
           SDL2.freeSurface bmp
           pure $ texture

         cachedLoad : Renderer -> ImageCache -> String -> IO (Texture, ImageCache)
         cachedLoad renderer cache name'
           = case lookup name' cache of
                  Nothing => do image <- loadTexture renderer (nameToFilepath name')
                                pure (image, insert name' image cache)
                  Just image => pure (image, cache)

  drawTexture draw texture src dst
    = with ST do [renderer, imageCache] <- split draw
                 res <- lift $ SDL2.renderCopy' !(read renderer) texture src dst
                 combine draw [renderer, imageCache]
                 pure res

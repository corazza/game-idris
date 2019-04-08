import Data.SortedMap
import Control.ST
import Graphics.SDL2 as SDL2

import Events
import Scene
import Objects
import Input
import Resources

%access public export

ImageCache : Type
ImageCache = SortedMap String Texture

emptyImageCache : ImageCache
emptyImageCache = empty

interface Draw (m : Type -> Type) where
  SDraw : Type

  initDraw : Int -> Int -> ST m Var [add SDraw]
  quitDraw : (draw : Var) -> ST m () [remove draw SDraw]

  poll : ST m (Either () (List InputEvent)) []

  clear : (draw : Var) -> ST m Int [draw ::: SDraw]
  present : (draw : Var) -> ST m () [draw ::: SDraw]

  getTexture : (draw : Var) -> (name : String) -> ST m Texture [draw ::: SDraw]
  loadTexture : (draw : Var) -> (name : String) -> ST m Texture [draw ::: SDraw]
  destroyTexture : Texture -> STrans m () xs (const xs)

  drawTexture : (draw : Var) ->
                (texture : Texture) ->
                (src : Maybe SDLRect) ->
                (dst : Maybe SDLRect) ->
                ST m Int [draw ::: SDraw]

  drawWholeCenter : (draw : Var) ->
                    (texture : Texture) ->
                    (dst : SDLRect) ->
                    (angle : Double) ->
                    ST m () [draw ::: SDraw]

public export
Draw m => Loader m Texture where
  Context {m} = SDraw {m}
  idToFilepath id = "res/images/" ++ id ++ ".bmp"
  loadFilepath ctx filepath = with ST do
    texture <- loadTexture ctx filepath
    pure (Just texture)
  destroy = destroyTexture

export
Draw IO where
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

  poll = lift pollEvents >>= pure . processEvents

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

  loadTexture draw filepath = with ST do
    [renderer, imageCache] <- split draw
    bmp <- lift $ SDL2.loadBMP filepath
    texture <- lift $ SDL2.createTextureFromSurface !(read renderer) bmp
    lift $ SDL2.freeSurface bmp
    combine draw [renderer, imageCache]
    pure texture

  destroyTexture texture = lift $ SDL2.destroyTexture texture

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

  drawWholeCenter draw texture dst angle = with ST do
    [renderer, imageCache] <- split draw
    lift $ SDL2.drawWholeCenter !(read renderer) texture dst angle
    combine draw [renderer, imageCache]

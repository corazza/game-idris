import Data.AVL.Dict
import Control.ST
import Graphics.SDL2 as SDL2

import Events
import Objects
import Input
import Resources
import Common

%access public export

ImageCache : Type
ImageCache = CacheType Texture

emptyImageCache : ImageCache
emptyImageCache = empty

loadTexture : (renderer : Renderer) -> (filepath : String) -> IO (Maybe Texture)
loadTexture renderer filepath = with IO do
  Just surface <- SDL2.loadImage filepath | pure Nothing
  texture <- SDL2.createTextureFromSurface renderer surface
  SDL2.freeSurface surface
  pure (Just texture)

interface Draw (m : Type -> Type) where
  SDraw : Type

  initDraw : Int -> Int -> ST m Var [add SDraw]
  quitDraw : (draw : Var) -> ST m () [remove draw SDraw]

  poll : ST m (Either () (List InputEvent)) []

  clear : (draw : Var) -> ST m Int [draw ::: SDraw]
  present : (draw : Var) -> ST m () [draw ::: SDraw]

  getTexture : (draw : Var) -> (id : String) -> ST m (Maybe Texture) [draw ::: SDraw]
  destroyTexture : Texture -> STrans m () xs (const xs)

  filledRect : (draw : Var) -> (dst : SDLRect) -> Color -> ST m () [draw ::: SDraw]

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

-- TODO move to GameIO? like physics

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

  getTexture draw id = with ST do
    [renderer, imageCache] <- split draw
    cache <- read imageCache
    case lookup id cache of -- TODO this pattern is repeated from SimpleCache, fix
      Nothing => with ST do
        Just texture <- lift $ loadTexture !(read renderer) ("res/images/" ++ id)
                     | do combine draw [renderer, imageCache]; pure Nothing
        write imageCache (insert id texture cache)
        combine draw [renderer, imageCache]
        pure $ Just texture
      Just x => do combine draw [renderer, imageCache]; pure (Just x)

  destroyTexture texture = lift $ SDL2.destroyTexture texture

  filledRect draw (MkSDLRect x y z w) (MkColor r g b a) = with ST do
    [renderer, imageCache] <- split draw
    lift $ SDL2.filledRect !(read renderer) x y z w r g b a
    combine draw [renderer, imageCache]

  drawTexture draw texture src dst
    = with ST do [renderer, imageCache] <- split draw
                 res <- lift $ SDL2.renderCopy' !(read renderer) texture src dst
                 combine draw [renderer, imageCache]
                 pure res

  drawWholeCenter draw texture dst angle = with ST do
    [renderer, imageCache] <- split draw
    lift $ SDL2.drawWholeCenter !(read renderer) texture dst angle
    combine draw [renderer, imageCache]

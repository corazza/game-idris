import Data.AVL.Dict
import Language.JSON
import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL2 as SDL2

import GameIO
import Events
import Objects
import Input
import Resources
import Exception
import Common

%access public export

public export
record Animation where
  constructor MkAnimation
  sheet : ResourceReference
  dimensions : (Int, Int) -- width and height of single sprite in pixels
  nx : Int
  facingRight : Bool

ObjectCaster Animation where
  objectCast dict = with Checked do
    sheet <- getString "sheet" dict
    dimensions <- getIntPair "dimensions" dict
    nx <- getInt "nx" dict
    facingRight <- getBoolOrDefault True "facingRight" dict
    pure $ MkAnimation sheet dimensions nx facingRight

GameIO m => SimpleLoader m Animation where
  load id = checkedJSONLoad (refToFilepath id)

ImageCache : Type
ImageCache = CacheType Texture

AnimationCache : Type
AnimationCache = CacheType Animation

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

  -- MAYBE FIX wrong concern, this is just a JSON descriptor
  getAnimation : (draw : Var) -> (id : String) -> ST m (Checked Animation) [draw ::: SDraw]
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

  drawCenter : (draw : Var) -> (texture : Texture) ->
               (src : SDLRect) -> (dst : SDLRect) ->
               (angle : Double) -> (flip : Int) ->
               ST m () [draw ::: SDraw]

export
Draw IO where
  SDraw = Composite [State Renderer,
                     State ImageCache,
                     SCache {m=IO} {r=Animation}]

  initDraw x y = with ST do
    renderer <- new $ !(lift (init x y))
    imageCache <- new empty
    animationCache <- initCache {r=Animation}
    draw <- new ()
    combine draw [renderer, imageCache, animationCache]
    pure draw

  -- TODO: free images?
  quitDraw draw = with ST do
    [renderer, imageCache, animationCache] <- split draw
    lift quit
    delete renderer
    delete imageCache
    quitCache animationCache
    delete draw

  poll = lift pollEvents >>= pure . processEvents

  clear draw = with ST do
    [srenderer, imageCache, animationCache] <- split draw
    renderer <- read srenderer
    lift $ SDL2.setRendererDrawColor renderer 0 0 0 0
    lift $ SDL2.renderClear renderer
    combine draw [srenderer, imageCache, animationCache]
    pure 1 -- TODO: should process SDL2 errors

  present draw = with ST do
    [renderer, imageCache, animationCache] <- split draw
    lift $ SDL2.renderPresent !(read renderer)
    combine draw [renderer, imageCache, animationCache]

  getAnimation draw name = with ST do
    [renderer, imageCache, animationCache] <- split draw
    animation <- get animationCache name
    combine draw [renderer, imageCache, animationCache]
    pure animation

  getTexture draw id = with ST do
    [renderer, imageCache, animationCache] <- split draw
    cache <- read imageCache
    case lookup id cache of -- TODO this pattern is repeated from SimpleCache, fix
      Nothing => with ST do
        Just texture <- lift $ loadTexture !(read renderer) (refToFilepath id)
                     | do combine draw [renderer, imageCache, animationCache]; pure Nothing
        write imageCache (insert id texture cache)
        combine draw [renderer, imageCache, animationCache]
        pure $ Just texture
      Just x => do combine draw [renderer, imageCache, animationCache]; pure (Just x)

  destroyTexture texture = lift $ SDL2.destroyTexture texture

  filledRect draw (MkSDLRect x y z w) (MkColor r g b a) = with ST do
    [renderer, imageCache, animationCache] <- split draw
    lift $ SDL2.filledRect !(read renderer) x y z w r g b a
    combine draw [renderer, imageCache, animationCache]

  drawTexture draw texture src dst
    = with ST do [renderer, imageCache, animationCache] <- split draw
                 res <- lift $ SDL2.renderCopy' !(read renderer) texture src dst
                 combine draw [renderer, imageCache, animationCache]
                 pure res

  drawWholeCenter draw texture dst angle = with ST do
    [renderer, imageCache, animationCache] <- split draw
    lift $ SDL2.drawWholeCenter !(read renderer) texture dst angle
    combine draw [renderer, imageCache, animationCache]

  drawCenter draw texture src dst angle flip = with ST do
    [renderer, imageCache, animationCache] <- split draw
    lift $ SDL2.drawCenter !(read renderer) texture src dst angle flip
    combine draw [renderer, imageCache, animationCache]

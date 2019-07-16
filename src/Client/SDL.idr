module Client.SDL

import Data.AVL.Dict
import Control.ST
import Control.ST.ImplicitCall
import public Graphics.SDL2 as SDL2

import public Descriptions.Color
import Exception
import GameIO

TextureCache : Type
TextureCache = Dict ResourceReference Texture

loadTexture : (renderer : Renderer) -> (filepath : String) -> IO (Checked Texture)
loadTexture renderer filepath = with IO do
  Just surface <- SDL2.loadImage filepath
               | pure (fail $ "failed to load " ++ filepath ++ " (SDL.idr)")
  texture <- SDL2.createTextureFromSurface renderer surface
  SDL2.freeSurface surface
  pure $ pure texture

public export
interface SDL (m : Type -> Type) where
  SSDL : Type

  startSDL : Int -> Int -> ST m Var [add SSDL]
  endSDL : (draw : Var) -> ST m () [remove draw SSDL]

  poll : STrans m (List SDL2.Event) xs (const xs)

  clear : (draw : Var) -> ST m Int [draw ::: SSDL]
  present : (draw : Var) -> ST m () [draw ::: SSDL]

  -- TODO the SDL lib should obviously accept Maybe SDLRect so these two methods
  -- could be unified
  drawWholeCenter : (draw : Var) ->
                    (ref : ResourceReference) ->
                    (dst : SDLRect) ->
                    (angle : Double) ->
                    (flip : Int) ->
                    ST m () [draw ::: SSDL]

  drawCenter : (draw : Var) ->
               (ref : ResourceReference) ->
               (src : SDLRect) ->
               (dst : SDLRect) ->
               (angle : Double) ->
               (flip : Int) ->
               ST m () [draw ::: SSDL]

  filledRect : (draw : Var) -> (dst : SDLRect) -> Color -> ST m () [draw ::: SSDL]

  -- TODO resetCache : ...

  private
  getTexture : (sdl : Var) -> (ref : ResourceReference) -> ST m (Checked Texture) [sdl ::: SSDL]

export
SDL IO where
  SSDL = Composite [State Renderer, State TextureCache]

  startSDL x y = with ST do
    renderer <- new $ !(lift (init x y))
    textureCache <- new empty
    sdl <- new ()
    combine sdl [renderer, textureCache]
    pure sdl

  endSDL sdl = with ST do
    [renderer, textureCache] <- split sdl
    lift quit
    delete renderer; delete textureCache
    delete sdl

  poll = lift pollEvents

  clear sdl = with ST do
    [renderer, imageCache] <- split sdl
    renderer' <- read renderer
    lift $ SDL2.setRendererDrawColor renderer' 0 0 0 0
    lift $ SDL2.renderClear renderer'
    combine sdl [renderer, imageCache]
    pure 1 -- TODO: should process SDL2 errors

  present sdl = with ST do
    [renderer, imageCache] <- split sdl
    lift $ SDL2.renderPresent !(read renderer)
    combine sdl [renderer, imageCache]

  drawWholeCenter sdl ref dst angle flip = with ST do
    Right texture <- getTexture sdl ref | Left e => lift (log e)
    [renderer, imageCache] <- split sdl
    lift $ SDL2.drawWholeCenter !(read renderer) texture dst angle flip
    combine sdl [renderer, imageCache]

  drawCenter sdl ref src dst angle flip = with ST do
    Right texture <- getTexture sdl ref | Left e => lift (log e)
    [renderer, imageCache] <- split sdl
    lift $ SDL2.drawCenter !(read renderer) texture src dst angle flip
    combine sdl [renderer, imageCache]

  filledRect sdl (MkSDLRect x y z w) (MkColor r g b a) = with ST do
    [renderer, imageCache] <- split sdl
    lift $ SDL2.filledRect !(read renderer) x y z w r g b a
    combine sdl [renderer, imageCache]

  getTexture sdl ref = with ST do
    [renderer, imageCache] <- split sdl
    cache <- read imageCache
    case lookup ref cache of
      Nothing => with ST do
        Right texture <- lift $ loadTexture !(read renderer) (refToFilepath ref)
                      | Left e => with ST do
                            combine sdl [renderer, imageCache]
                            pure $ fail e
        update imageCache (insert ref texture)
        combine sdl [renderer, imageCache]
        pure $ Right texture
      Just texture => do combine sdl [renderer, imageCache]; pure (Right texture)

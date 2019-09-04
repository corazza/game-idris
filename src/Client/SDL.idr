module Client.SDL

import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Graphics.SDL2 as SDL2

import Client.SDL.PSDL
import Descriptions.Color
import Descriptions.FontDescription
import Exception
import GameIO
import JSONCache

TextureCache : Type
TextureCache = Dict String Texture

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

  startSDL : Int -> Int -> PreloadResults -> ST m Var [add SSDL]
  endSDL : (draw : Var) -> ST m () [remove draw SSDL]

  poll : STrans m (List SDL2.Event) xs (const xs)

  clear : (draw : Var) -> ST m Int [draw ::: SSDL]
  present : (draw : Var) -> ST m () [draw ::: SSDL]

  -- TODO the SDL lib should obviously accept Maybe SDLRect so these two methods
  -- could be unified
  -- TODO FIX URGENT flipping logic should be handled here, not in game code!!!!
  drawWholeCenter : (draw : Var) ->
                    (ref : ContentReference) ->
                    (dst : SDLRect) ->
                    (angle : Double) ->
                    (flip : Int) ->
                    ST m () [draw ::: SSDL]

  drawCenter : (draw : Var) ->
               (ref : ContentReference) ->
               (src : SDLRect) ->
               (dst : SDLRect) ->
               (angle : Double) ->
               (flip : Int) ->
               ST m () [draw ::: SSDL]

  filledRect : (draw : Var) -> (dst : SDLRect) -> Color -> ST m () [draw ::: SSDL]
  drawText : (draw : Var) ->
             (text : String) ->
             (font_desc_ref : ContentReference) ->
             (dst : SDLRect) ->
             ST m () [draw ::: SSDL]

  -- TODO resetCache : ...

  private
  getTexture : (sdl : Var) -> (ref : ContentReference) -> ST m (Checked Texture) [sdl ::: SSDL]

export
SDL IO where
  SSDL = Composite [State Renderer, State TextureCache, State PSDL]

  startSDL x y preload = with ST do
    renderer <- new $ !(lift (init x y))
    textureCache <- new empty
    sdl <- new ()
    psdl <- new $ MkPSDL preload
    combine sdl [renderer, textureCache, psdl]
    pure sdl

  endSDL sdl = with ST do
    [renderer, textureCache, psdl] <- split sdl
    lift quit
    delete renderer; delete textureCache; delete psdl
    delete sdl

  poll = lift pollEvents

  clear sdl = with ST do
    [renderer, textureCache, psdl] <- split sdl
    renderer' <- read renderer
    lift $ SDL2.setRendererDrawColor renderer' 0 0 0 0
    lift $ SDL2.renderClear renderer'
    combine sdl [renderer, textureCache, psdl]
    pure 1 -- TODO: should process SDL2 errors

  present sdl = with ST do
    [renderer, textureCache, psdl] <- split sdl
    lift $ SDL2.renderPresent !(read renderer)
    combine sdl [renderer, textureCache, psdl]

  drawWholeCenter sdl ref dst angle flip = with ST do
    Right texture <- getTexture sdl ref | Left e => lift (log e)
    [renderer, textureCache, psdl] <- split sdl
    lift $ SDL2.drawWholeCenter !(read renderer) texture dst angle flip
    combine sdl [renderer, textureCache, psdl]

  drawCenter sdl ref src dst angle flip = with ST do
    Right texture <- getTexture sdl ref | Left e => lift (log e)
    [renderer, textureCache, psdl] <- split sdl
    lift $ SDL2.drawCenter !(read renderer) texture src dst angle flip
    combine sdl [renderer, textureCache, psdl]

  filledRect sdl (MkSDLRect x y z w) (MkColor r g b a) = with ST do
    [renderer, textureCache, psdl] <- split sdl
    lift $ SDL2.filledRect !(read renderer) x y z w r g b a
    combine sdl [renderer, textureCache, psdl]

  drawText sdl text font_desc_ref rect = with ST do
    [renderer, textureCache, psdl] <- split sdl
    psdl' <- read psdl
    let preload = preload psdl'
    case getFontDescription font_desc_ref preload of
      Left e => with ST do
        lift $ log $ "couldn't get font description for " ++ font_desc_ref ++ ", error:"
        lift $ log e
        combine sdl [renderer, textureCache, psdl]
      Right (MkFontDescription ref size color) => with ST do
        let (MkColor r g b a) = color
        let path = "res/" ++ ref
        lift $ SDL2.renderText !(read renderer) text path size (r, g, b, a) rect
        combine sdl [renderer, textureCache, psdl]

  getTexture sdl ref = with ST do
    [renderer, textureCache, psdl] <- split sdl
    cache <- read textureCache
    case lookup ref cache of
      Nothing => with ST do
        Right texture <- lift $ loadTexture !(read renderer) (refToFilepath ref)
                      | Left e => with ST do
                            combine sdl [renderer, textureCache, psdl]
                            pure $ fail e
        update textureCache (insert ref texture)
        combine sdl [renderer, textureCache, psdl]
        pure $ Right texture
      Just texture => do combine sdl [renderer, textureCache, psdl]; pure (Right texture)

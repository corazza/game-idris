module Resources

import Data.SortedMap

import Graphics.SDL2

-- for width/height https://wiki.libsdl.org/SDL_QueryTexture
public export
record Image where
  constructor MkImage
  texture : Texture
  width, height : Int

public export
record Sound where
  constructor MkSound
  length : Double

resourceType : String -> Type

loadImage : Renderer -> (filepath : String) -> IO Image
loadImage renderer filepath = do
  bmp <- SDL2.loadBMP  filepath -- XXX: handle errors
{-
  case bmp of
         Surface ptr => if ptr == null then abort "loadBMP" else pure ()
-}
  texture <- SDL2.createTextureFromSurface renderer bmp -- XXX: handle errors
  SDL2.freeSurface bmp
  pure $ MkImage texture 0 0


export
ImageCache : Type
ImageCache = SortedMap String Image

export
emptyImageCache : ImageCache
emptyImageCache = empty

nameToFilepath : (name : String) -> String
nameToFilepath name = "res/images/" ++ name ++ ".bmp"

export
getImage : Renderer -> ImageCache -> (name : String) -> IO (Image, ImageCache)
getImage renderer cache name
  = case lookup name cache of
         Nothing    => do image <- loadImage renderer (nameToFilepath name)
                          pure (image, insert name image cache)
         Just image => pure (image, cache)

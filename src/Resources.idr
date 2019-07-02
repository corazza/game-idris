module Resources

import Data.AVL.Dict
import Graphics.SDL2 as SDL2
import Language.JSON
import Control.ST
import Control.ST.ImplicitCall

import Exception
import GameIO

public export
CacheType : Type -> Type
CacheType what = Dict String what

public export
emptyCache : (a : Type) -> CacheType a
emptyCache a = the (CacheType a) empty

-- get : CacheType r ->

public export
interface SimpleLoader (m : Type -> Type) r where
  load : (filepath : String) -> m (Checked r)

public export
interface SimpleCache (m : Type -> Type) r where
  SCache : Type
  initCache : ST m Var [add SCache]
  quitCache : (cache : Var) -> ST m () [remove cache SCache]
  get : (cache : Var) -> (id : String) -> ST m (Checked r) [cache ::: SCache]

public export
(GameIO m, SimpleLoader m r) => SimpleCache m r where
  SCache {r} = State $ CacheType r

  initCache = with ST do
    cache <- new (emptyCache r)
    pure cache

  quitCache cache = delete cache

  get cache id = with ST do
    dict <- read cache
    case lookup id dict of
      Nothing => with ST do
        Right result <- lift $ load {m} {r} id
                     | Left e => pure (fail e)
        write cache (insert id result dict)
        pure $ Right result
      Just x => pure $ Right x

module Resources

import Data.AVL.Dict
import Graphics.SDL2 as SDL2
import Language.JSON
import Control.ST


public export
CacheType : Type -> Type
CacheType what = Dict String what

public export
emptyCache : (a : Type) -> CacheType a
emptyCache a = the (CacheType a) empty

public export
interface Loader (m : Type -> Type) r where
  Context : Type
  idToFilepath : (id : String) -> String
  loadFilepath : (context : Var) ->
                 (filepath : String) ->
                 ST m (Maybe r) [context ::: Context]
  destroy : r -> STrans m () xs (const xs)

public export
interface Cache (m : Type -> Type) r where
  SCache : Type
  LoadContext : Type

  initCache : ST m Var [add SCache]
  quitCache : (cache : Var) -> ST m () [remove cache SCache]

  get : (cache : Var) ->
        (context : Var) ->
        (id : String) ->
        ST m (Maybe r) [cache ::: SCache, context ::: LoadContext]

public export
(Loader m r, ConsoleIO m) => Cache m r where
  -- SCache = ?what
  SCache {r} = State $ CacheType r
  LoadContext {m} {r} = Context {m} {r}

  initCache = with ST do
    cache <- new (emptyCache r)
    pure cache

  quitCache cache = (with ST do
    dict <- read cache
    destroyAll (values dict)
    delete cache) where
      destroyAll : List r -> STrans m () xs (const xs)
      destroyAll [] = pure ()
      destroyAll (x :: xs) = destroy x >>= (\_ => destroyAll xs)

  get cache ctx id = with ST do
    dict <- read cache
    case lookup id dict of
      Nothing => do let filepath = idToFilepath {m} {r} id
                    Just result <- call $ loadFilepath {m} {r} ctx filepath | pure Nothing
                    write cache (insert id result dict)
                    pure (Just result)
      x => pure x

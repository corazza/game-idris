module JSONCache

import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Graphics.SDL2 as SDL2
import Language.JSON
import Physics.Vector2D -- needed only for empty/dummy/default objects REMOVE

import Exception
import GameIO
import Descriptions

public export
record Preload where
  constructor MkPreload
  objects : List ContentReference
  walls : List ContentReference
  animations : List ContentReference
  maps : List ContentReference
%name Preload preload

export
Show Preload where
  show (MkPreload objects walls animations maps)
    =  "{ objects: " ++ show objects
    ++ ", walls: " ++ show walls
    ++ ", animations: " ++ show animations
    ++ ", maps: " ++ show maps
    ++ " }"

export
ObjectCaster Preload where
  objectCast dict = with Checked do
    objects <- getStrings "objects" dict
    walls <- getStrings "walls" dict
    animations <- getStrings "animations" dict
    maps <- getStrings "maps" dict
    pure $ MkPreload objects walls animations maps

public export
CacheType : Type -> Type
CacheType ty = Dict ContentReference ty

public export
record PreloadResults where
  constructor MkPreloadResults
  objects : CacheType ObjectDescription
  walls : CacheType WallDescription
  animations : CacheType AnimationDescription
  maps : CacheType MapDescription
%name PreloadResults preload

export
emptyPreloadResults : PreloadResults
emptyPreloadResults = MkPreloadResults empty empty empty empty

lookupError : (ref : ContentReference) -> (type : String) -> String
lookupError type ref = "can't find " ++ type ++ " " ++ show ref

export
getObjectDescription : (ref : ContentReference) ->
                       (preload : PreloadResults) ->
                       Checked ObjectDescription
getObjectDescription ref
  = maybeToEither (lookupError "object description" ref) . lookup ref . objects

export
getWallDescription : (ref : ContentReference) ->
                     (preload : PreloadResults) ->
                     Checked WallDescription
getWallDescription ref
  = maybeToEither (lookupError "wall description" ref) . lookup ref . walls

export
getAnimationDescription : (ref : ContentReference) ->
                          (preload : PreloadResults) ->
                          Checked AnimationDescription
getAnimationDescription ref
  = maybeToEither (lookupError "animation description" ref) . lookup ref . animations

export
getMapDescription : (ref : ContentReference) ->
                    (preload : PreloadResults) ->
                    Checked MapDescription
getMapDescription ref
  = maybeToEither (lookupError "map description" ref) . lookup ref . maps

wallCreationToDescPair : PreloadResults -> WallCreation -> Checked (WallCreation, WallDescription)
wallCreationToDescPair preload wall_creation
  = map (MkPair wall_creation) $ getWallDescription (ref wall_creation) preload

getWallsFromMap : PreloadResults -> MapDescription -> Checked (List (WallCreation, WallDescription))
getWallsFromMap preload desc = catResults $ map (wallCreationToDescPair preload) (walls desc)

wallPairToObjectPair : PreloadResults ->
                       (WallCreation, WallDescription) ->
                       Checked (WallCreation, ObjectDescription)
wallPairToObjectPair preload (wall_creation, wall_description)
  = (wallDescToObjectDesc wall_description (wall_data wall_creation)) >>=
        pure . MkPair wall_creation

export
getWallsAsObjects : PreloadResults -> MapDescription -> Checked (List (WallCreation, ObjectDescription))
getWallsAsObjects preload desc = getWallsFromMap preload desc >>=
  catResults . map (wallPairToObjectPair preload)

creationToDescPair : PreloadResults -> Creation -> Checked (Creation, ObjectDescription)
creationToDescPair preload creation
  = map (MkPair creation) $ getObjectDescription (ref creation) preload

export
getObjectsFromMap : PreloadResults -> MapDescription -> Checked (List (Creation, ObjectDescription))
getObjectsFromMap preload desc = catResults $ map (creationToDescPair preload) $ creations desc

public export
interface JSONCache (m : Type -> Type) where
  SJSONCache : Type -> Type

  startCache : ObjectCaster r => List ContentReference -> ST m Var [add (SJSONCache r)]
  endCache : ObjectCaster r => (cache : Var) -> ST m () [remove cache (SJSONCache r)]

  preload : ObjectCaster r => (cache : Var) -> (ref : ContentReference) -> ST m () [cache ::: SJSONCache r]
  preloads : ObjectCaster r => (cache : Var) -> (refs : List ContentReference) -> ST m () [cache ::: SJSONCache r]

  getCache : ObjectCaster r => (cache : Var) -> ST m (CacheType r) [cache ::: SJSONCache r]

export
GameIO m => JSONCache m where
  SJSONCache r = State (CacheType r)

  startCache refs = with ST do
    cache <- new empty
    preloads cache refs
    pure cache

  endCache cache = delete cache

  preload cache ref = with ST do
    Right loaded <- lift $ checkedJSONLoad {r=r} ("res/" ++ ref)
                 | Left e => with ST do
                    lift $ log $ "couldn't load " ++ ref ++ ", error:"
                    lift $ log $ e
    update cache (insert ref loaded)

  preloads cache [] = pure ()
  preloads cache (ref::refs) = preload cache ref >>= const (preloads cache refs)

  getCache cache = read cache >>= pure


preloadDict : GameIO m => ObjectCaster r => (refs : List ContentReference) -> ST m (CacheType r) []
preloadDict {r} refs = with ST do
  cache <- startCache {r=r} refs
  dict <- getCache {r=r} cache
  endCache {r=r} cache
  pure dict

export
preloadResults : GameIO m => (preload_info : Preload) -> ST m PreloadResults []
preloadResults preload_info = with ST do
  objects_dict <- preloadDict {r=ObjectDescription} (objects preload_info)
  walls_dict <- preloadDict {r=WallDescription} (walls preload_info)
  animations_dict <- preloadDict {r=AnimationDescription} (animations preload_info)
  maps_dict <- preloadDict {r=MapDescription} (maps preload_info)
  pure $ MkPreloadResults objects_dict walls_dict animations_dict maps_dict

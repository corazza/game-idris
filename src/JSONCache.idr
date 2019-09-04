module JSONCache

import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Graphics.SDL2 as SDL2
import Language.JSON
import Physics.Vector2D -- needed only for empty/dummy/default objects REMOVE

import Exception
import GameIO
import Objects
import Descriptions.ObjectDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Descriptions.MapDescription
import Descriptions.ItemDescription
import Descriptions.WallDescription
import Descriptions.AnimationDescription
import Descriptions.NumericPropertyRender
import Descriptions.SurfaceDescription
import Descriptions.FontDescription

public export
record Preload where
  constructor MkPreload
  objects : List ContentReference
  walls : List ContentReference
  animations : List ContentReference
  maps : List ContentReference
  behaviors : List ContentReference
  items : List ContentReference
  ui : List ContentReference
  fonts : List ContentReference
%name Preload preload

export
Show Preload where
  show (MkPreload objects walls animations maps behaviors items ui fonts)
    =  "{ objects: " ++ show objects
    ++ ", walls: " ++ show walls
    ++ ", animations: " ++ show animations
    ++ ", maps: " ++ show maps
    ++ ", behaviors: " ++ show behaviors
    ++ ", items: " ++ show items
    ++ ", ui: " ++ show ui
    ++ ", fonts: " ++ show fonts
    ++ " }"

export
ObjectCaster Preload where
  objectCast dict = with Checked do
    objects <- getStrings "objects" dict
    walls <- getStrings "walls" dict
    animations <- getStrings "animations" dict
    maps <- getStrings "maps" dict
    behaviors <- getStrings "behaviors" dict
    items <- getStrings "items" dict
    ui <- getStrings "ui" dict
    fonts <- getStrings "fonts" dict
    pure $ MkPreload objects walls animations maps behaviors items ui fonts

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
  behaviors : CacheType BehaviorDescription
  items : CacheType ItemDescription
  ui : CacheType SurfaceDescription
  fonts : CacheType FontDescription
  numPropRender : NumPropRenderDescriptionDict
%name PreloadResults preload

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
getItemDescription : (ref : ContentReference) ->
                     (preload : PreloadResults) ->
                     Checked ItemDescription
getItemDescription ref
  = maybeToEither (lookupError "item description" ref) . lookup ref . items

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

export
getBehaviorDescription : (ref : ContentReference) ->
                         (preload : PreloadResults) ->
                         Checked BehaviorDescription
getBehaviorDescription ref
  = maybeToEither (lookupError "behavior description" ref) . lookup ref . behaviors

export
getSurfaceDescription : (ref : ContentReference) ->
                        (preload : PreloadResults) ->
                        Checked SurfaceDescription
getSurfaceDescription ref
  = maybeToEither (lookupError "ui description" ref) . lookup ref . ui

export
getFontDescription : (ref : ContentReference) ->
                     (preload : PreloadResults) ->
                     Checked FontDescription
getFontDescription ref
  = maybeToEither (lookupError "font description" ref) . lookup ref . fonts

export
getNumPropRender : (id : NumericPropertyId) ->
                   (preload : PreloadResults) ->
                   Checked NumPropRenderDescription
getNumPropRender id preload
  = let error = "couldn't find numeric property render description with id " ++ id
        in maybeToEither error $ lookup id $ numPropRender preload

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

preloadNumProp : GameIO m => ST m (Checked NumPropRenderDescriptionDict) []
preloadNumProp = lift
               $ checkedJSONLoad {r=NumPropRenderDescriptionDict}
               $ refToFilepath "main/numeric_properties.json"

export
preloadResults : GameIO m => (preload_info : Preload) -> ST m (Checked PreloadResults) []
preloadResults preload_info = with ST do
  objects_dict <- preloadDict {r=ObjectDescription} (objects preload_info)
  walls_dict <- preloadDict {r=WallDescription} (walls preload_info)
  animations_dict <- preloadDict {r=AnimationDescription} (animations preload_info)
  maps_dict <- preloadDict {r=MapDescription} (maps preload_info)
  behaviors_dict <- preloadDict {r=BehaviorDescription} (behaviors preload_info)
  items_dict <- preloadDict {r=ItemDescription} (items preload_info)
  ui_dict <- preloadDict {r=SurfaceDescription} (ui preload_info)
  fonts_dict <- preloadDict {r=FontDescription} (fonts preload_info)
  Right numPropRender <- preloadNumProp
        | Left e => pure (fail e)
  pure $ Right $ MkPreloadResults
    objects_dict walls_dict animations_dict maps_dict behaviors_dict items_dict
    ui_dict fonts_dict numPropRender

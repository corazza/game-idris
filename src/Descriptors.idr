module SceneDescriptor

import Language.JSON
import Physics.Vector2D
import Control.ST
import Data.AVL.Dict

import Resources
import GameIO



interface ObjectCaster a where
  objectCast : Dict String JSON -> Maybe a

ObjectCaster a => Cast JSON (Maybe a) where
  cast (JObject xs) = objectCast (fromList xs)
  cast _ = Nothing


getVector : (name : String) -> (dict : Dict String JSON) -> Maybe Vector2D
getVector name dict = with Maybe do
  JArray [JNumber x, JNumber y] <- lookup name dict | Nothing
  pure (x, y)


public export
data RenderDescriptor
  = DrawBox ResourceReference
  | TileWith ResourceReference Int Int

ObjectCaster RenderDescriptor where
  objectCast dict = with Maybe do
    JString type <- lookup "type" dict | Nothing
    JString image <- lookup "image" dict | Nothing
    case type of
         "single" => pure $ DrawBox image
         "tile" => pure $ TileWith image 0 0 -- TODO read two ints
         _ => Nothing

public export
data BodyType = Wall | Box

Cast String (Maybe BodyType) where
  cast "wall" = Just Wall
  cast "box" = Just Box
  cast _ = Nothing

public export
record BodyDescriptor where
  constructor MkBodyDescriptor
  type : BodyType
  mass : Maybe Double
  dimensions : Maybe Vector2D -- for walls, added later

%name BodyDescriptor desc

public export
physicsMass : BodyDescriptor -> Double
physicsMass desc = case mass desc of
  Nothing => 0
  Just x => x

ObjectCaster BodyDescriptor where
  objectCast dict = (with Maybe do
    JString typeString <- lookup "type" dict | Nothing
    type <- getType typeString | Nothing
    pure $ MkBodyDescriptor type (getMass dict) (getVector "dimensions" dict)) where
      getType : String -> Maybe BodyType
      getType = cast

      getMass : Dict String JSON -> Maybe Double
      getMass x = with Maybe do
        JNumber mass <- lookup "mass" x | Nothing
        pure mass



public export
data ObjectTag = Spawn

Cast String (Maybe ObjectTag) where
  cast "spawn" = Just Spawn
  cast _ = Nothing

getTags : Dict String JSON -> List ObjectTag
getTags dict = (case lookup "tags" dict of
  Nothing => []
  Just (JArray xs) => catMaybes $ map cast (getStrings xs)
  Just _ => []) where
    getStrings : List JSON -> List String
    getStrings [] = []
    getStrings (JString x :: xs) = x :: getStrings xs
    getStrings (_ :: xs) = getStrings xs



public export
record ObjectDescriptor where
  constructor MkObjectDescriptor
  name : String
  bodyDescription : BodyDescriptor
  renderDescription : RenderDescriptor
  tags : List ObjectTag


-- all objects are created equally / flat, by passing a Creation with a ResourceReference
-- to the scene, which then loads the required ObjectDescriptor and instantiates
-- the object

||| Collects a flat description of a scene object creation for later processing
public export
record Creation where
  constructor MkCreation
  id : Maybe String
  ref : ResourceReference
  position : Vector2D
  tags : List ObjectTag
  velocity : Maybe Vector2D
  dimensions : Maybe Vector2D

public export
record MapDescriptor where
  constructor MkMapDescriptor
  name : String
  background : ResourceReference
  creations : List Creation

ObjectCaster Creation where
  objectCast dict = with Maybe do
    JString ref <- lookup "ref" dict | Nothing
    JArray [JNumber x, JNumber y] <- lookup "position" dict | Nothing
    pure $ MkCreation Nothing
                      ref
                      (x, y)
                      (getTags dict)
                      (getVector "velocity" dict)
                      (getVector "dimensions" dict)

ObjectCaster MapDescriptor where
  objectCast dict = with Maybe do
    JString name <- lookup "name" dict | Nothing
    JString background <- lookup "background" dict | Nothing
    JArray creations <- lookup "creations" dict | Nothing
    pure $ MkMapDescriptor name background (catMaybes (map cast creations))


randomRender : RenderDescriptor
randomRender = DrawBox "a"

ObjectCaster ObjectDescriptor where
  objectCast dict = with Maybe do
    JString name <- lookup "name" dict | Nothing
    box2dJson <- lookup "box2d" dict | Nothing
    box2d <- (the (Maybe BodyDescriptor) (cast box2dJson)) | Nothing
    renderJson <- lookup "render" dict | Nothing
    render <- (the (Maybe RenderDescriptor) (cast renderJson)) | Nothing
    pure $ MkObjectDescriptor name box2d render (getTags dict)

public export
jsonloadFilepath : (Monad m, Cast JSON (Maybe r), GameIO m, ConsoleIO m) =>
                   (filepath : String) ->
                   STrans m (Maybe r) xs (const xs)
jsonloadFilepath filepath = with ST do
  Just a <- lift $ loadJSON filepath | pure Nothing
  pure $ cast a

public export
(Monad m, GameIO m, ConsoleIO m) => Loader m MapDescriptor where
  Context {m} = EmptyContext {m}
  idToFilepath id = "res/maps/" ++ id ++ ".json"
  loadFilepath _ = jsonloadFilepath
  destroy _ = pure ()
  -- loadFilepath _ filepath = do
  --   Just a <- lift $ loadJSON filepath | pure Nothing
  --   pure $ cast a

public export
(Monad m, GameIO m, ConsoleIO m) => Loader m ObjectDescriptor where
  Context {m} = EmptyContext {m}
  idToFilepath id = "res/objects/" ++ id ++ ".json"
  loadFilepath _ = jsonloadFilepath
  destroy _ = pure ()
  -- loadFilepath _ filepath = do
  --   Just a <- lift $ loadJSON filepath | pure Nothing
  --   pure $ cast a

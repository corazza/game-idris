module SceneDescriptor

import Language.JSON
import Physics.Vector2D
import Control.ST
import Data.AVL.Dict

import Resources
import GameIO
import Common


interface ObjectCaster a where
  objectCast : Dict String JSON -> Maybe a

ObjectCaster a => Cast JSON (Maybe a) where
  cast (JObject xs) = objectCast (fromList xs)
  cast _ = Nothing

getVector : (name : String) -> (dict : Dict String JSON) -> Maybe Vector2D
getVector name dict = with Maybe do
  JArray [JNumber x, JNumber y] <- lookup name dict | Nothing
  pure (x, y)

getDouble : String -> Dict String JSON -> Maybe Double
getDouble key dict = case lookup key dict of
  Just (JNumber x) => Just x
  _ => Nothing

getDoubleOrDefault : String -> Double -> Dict String JSON -> Double
getDoubleOrDefault key default dict = case lookup key dict of
  Just (JNumber x) => x
  _ => default


public export
data IncompleteRenderDescriptor
  = DrawBox ResourceReference
  | TileWith ResourceReference Vector2D
  | Invisible

ObjectCaster IncompleteRenderDescriptor where
  objectCast dict = with Maybe do
    JString type <- lookup "type" dict | Nothing
    case type of
      "invisible" => pure Invisible
      "single" => with Maybe do
        JString image <- lookup "image" dict | Nothing
        pure $ DrawBox image
      "tile" => with Maybe do
        JString image <- lookup "image" dict | Nothing
        tileDims <- getVector "tileDims" dict | Nothing
        pure $ TileWith image tileDims
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
  density : Double
  friction : Double
  dimensions : Maybe Vector2D -- nonexistent for walls
%name BodyDescriptor desc

getType : String -> Maybe BodyType
getType = cast

ObjectCaster BodyDescriptor where
  objectCast dict = with Maybe do
    JString typeString <- lookup "type" dict | Nothing
    type <- getType typeString | Nothing
    pure $ MkBodyDescriptor type
                            (getDoubleOrDefault "density" 1 dict)
                            (getDoubleOrDefault "friction" 1 dict)
                            (getVector "dimensions" dict)

public export -- additional parameters supplied later
data ScriptDescriptor = Create ResourceReference

ObjectCaster ScriptDescriptor where
  objectCast dict = case lookup "type" dict of
    Just (JString "create") => with Maybe do
      JString ref <- lookup "ref" dict | Nothing
      pure $ Create ref
    _ => Nothing

Show ScriptDescriptor where
  show (Create ref) = "create " ++ ref

-- idk why `map cast (lookup "attack" dict)` doesn't work
getScript : String -> Dict String JSON -> Maybe ScriptDescriptor
getScript name dict = case lookup name dict of
  Nothing => Nothing
  Just x => cast x



public export
record ControlDescriptor where
  constructor MkControlDescriptor
  speed : Double
  jump : Double
  -- attack : Maybe ScriptDescriptor
%name ControlDescriptor cdesc

export
Show ControlDescriptor where
  show (MkControlDescriptor speed jump) =
    "{speed: " ++ show speed ++ ", jump: " ++ show jump ++ "}"

ObjectCaster ControlDescriptor where
  objectCast dict = with Maybe do
    JNumber speed <- lookup "speed" dict | Nothing
    JNumber jump <- lookup "jump" dict | Nothing
    pure $ MkControlDescriptor speed jump

getControl : Maybe JSON -> Maybe ControlDescriptor
getControl Nothing = Nothing
getControl (Just x) = the (Maybe ControlDescriptor) (cast x)



-- TODO shouldn't be here, but in Objects
public export
data ObjectTag = Spawn | Projectile

export
Show ObjectTag where
  show Spawn = "spawn"
  show Projectile = "projectile"

Cast String (Maybe ObjectTag) where
  cast "spawn" = Just Spawn
  cast "projectile" = Just Projectile
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
  renderDescription : IncompleteRenderDescriptor
  tags : List ObjectTag
  health : Maybe Double
  control : Maybe ControlDescriptor
  attack : Maybe ScriptDescriptor

export
Show ObjectDescriptor where
  show (MkObjectDescriptor name bodyDescription renderDescription tags health control attack)
      =  "{ name: " ++ name
      ++ ", friction: " ++ show (friction bodyDescription)
      ++ ", density: " ++ show (density bodyDescription)
      ++ ", health: " ++ show health
      ++ ", control: " ++ show control
      ++ ", attack: " ++ show attack
      ++ " }"

ObjectCaster ObjectDescriptor where
  objectCast dict = with Maybe do
    JString name <- lookup "name" dict | Nothing
    box2dJson <- lookup "box2d" dict | Nothing
    box2d <- (the (Maybe BodyDescriptor) (cast box2dJson)) | Nothing
    renderJson <- lookup "render" dict | Nothing
    render <- (the (Maybe IncompleteRenderDescriptor) (cast renderJson)) | Nothing
    pure $ MkObjectDescriptor name box2d render
                              (getTags dict)
                              (getDouble "health" dict)
                              (getControl (lookup "control" dict))
                              (getScript "attack" dict)

public export
data CreationData = BoxData (Maybe Vector2D)
                  | WallData (Int, Int)
                  | InvisibleWallData Vector2D

||| Collects a flat description of a scene object creation for later processing
public export
record Creation where
  constructor MkCreation
  id : Maybe String
  ref : ResourceReference
  position : Vector2D
  tags : List ObjectTag
  creationData : CreationData
%name Creation creation

ObjectCaster Creation where
  objectCast dict = (with Maybe do
    let id = getId dict
    JString ref <- lookup "ref" dict | Nothing
    pos <- getVector "position" dict | Nothing
    creationData <- getCreationData dict | Nothing
    pure $ MkCreation id ref pos (getTags dict) creationData) where
      getId : Dict String JSON -> Maybe String
      getId dict = with Maybe do
        JString id <- lookup "id" dict | Nothing
        Just id

      extractWallData : Dict String JSON -> Maybe CreationData
      extractWallData dict = let repeat = lookup "repeat" dict
                                 dimensions = lookup "dimensions" dict in
                  case (repeat, dimensions) of
                    (Just (JArray [JNumber xn, JNumber yn]), Nothing) =>
                      Just $ WallData (cast xn, cast yn)
                    (Nothing, Just (JArray [JNumber x, JNumber y])) =>
                      Just $ InvisibleWallData (x, y)
                    _ => Nothing

      -- extractBoxData : Dict String JSON -> Maybe CreationData
      -- extractBoxData dict =

      getCreationData : Dict String JSON -> Maybe CreationData
      getCreationData dict = case extractWallData dict of
        Nothing => Just $ BoxData (getVector "impulse" dict)
        Just x => Just x

public export
record Background where
  constructor MkBackground
  image : ResourceReference
  dimensions : Vector2D

ObjectCaster Background where
  objectCast dict = with Maybe do
    JString ref <- lookup "image" dict | Nothing
    dimensions <- getVector "dimensions" dict | Nothing
    pure $ MkBackground ref dimensions

public export
record MapDescriptor where
  constructor MkMapDescriptor
  name : String
  background : Background
  creations : List Creation

ObjectCaster MapDescriptor where
  objectCast dict = with Maybe do
    JString name <- lookup "name" dict | Nothing
    background' <- lookup "background" dict | Nothing
    background <- the (Maybe Background) (cast background') | Nothing
    JArray creations <- lookup "creations" dict | Nothing
    pure $ MkMapDescriptor name background (catMaybes (map cast creations))


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

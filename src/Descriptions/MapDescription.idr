module Descriptions.MapDescription

import Physics.Box2D

import Descriptions.ObjectDescription
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.BodyDescription
import Descriptions.ObjectDescription.BodyFlags
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.ControlDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.JointDescription
import Descriptions.Color
import GameIO
import Exception
import Timeline
import Timeline.Items
import Objects

public export -- loaded by server, received by client
record Creation where
  constructor MkCreation
  ref : ContentReference
  position : Vector2D
  impulse : Maybe Vector2D
  creator : Maybe ObjectId
  angle : Maybe Double
  behavior : Maybe BehaviorParameters
  id : Maybe ObjectId
  render : Maybe RenderDescription
  body : Maybe BodyDescription
%name Creation creation

export
Show Creation where
  show creation = ref creation ++ " at " ++ show (position creation)

getCreationAngle : JSONDict -> Checked (Maybe Double)
getCreationAngle dict = case hasKey "angle" dict of
  False => pure Nothing
  True => getDouble "angle" dict >>= pure . Just

ObjectCaster Creation where
  objectCast dict = with Checked do
    ref <- getString "ref" dict
    position <- getVector "position" dict
    angle <- getCreationAngle dict
    behavior <- the (Checked (Maybe BehaviorParameters)) $ getCastableMaybe "behavior" dict
    id <- getStringMaybe "id" dict
    render <- the (Checked (Maybe RenderDescription)) $ getCastableMaybe "render" dict
    body <- the (Checked (Maybe BodyDescription)) $ getCastableMaybe "body" dict
    pure $ MkCreation ref position Nothing Nothing angle behavior id render body

Serialize Creation where
  toDict creation = with ST do
    creationObject <- makeObject
    addString creationObject "ref" $ ref creation
    addVector creationObject "position" $ position creation
    addDoubleMaybe creationObject "angle" $ angle creation
    addObjectMaybe creationObject "behavior" $ behavior creation
    addStringMaybe creationObject "id" $ id creation
    addObjectMaybe creationObject "render" $ render creation
    addObjectMaybe creationObject "body" $ body creation
    getDict creationObject

export
creationForEditor : (ref : ContentReference) ->
                    (position : Vector2D) ->
                    (id : Maybe ObjectId) ->
                    Creation
creationForEditor ref position id =
  MkCreation ref position Nothing Nothing Nothing Nothing id Nothing Nothing

makeCreationStatic : Creation -> Creation
makeCreationStatic = record { body $= map makeStatic }

setId : ObjectId -> Creation -> Creation
setId id' = record { id = Just id' }

export
creationBodyDescriptionToDefinition : Creation -> BodyDescription -> BodyDefinition
creationBodyDescriptionToDefinition creation desc = MkBodyDefinition
  (type desc) (position creation) (angle creation) (fixedRotation desc) (bullet desc)

fromBehavior : BehaviorParameters -> RulesDescription
fromBehavior bp
  = MkRulesDescription Nothing Nothing Nothing (Just bp) Nothing rulesType' where
      rulesType' : RulesType
      rulesType' = fromMaybe Inanimate $ rulesType bp

export
rulesDescFromCreation : Maybe RulesDescription -> Creation -> Maybe RulesDescription
rulesDescFromCreation Nothing creation = map fromBehavior (behavior creation)
rulesDescFromCreation (Just desc) creation = case behavior creation of
  Nothing => Just desc
  Just bp => Just (record { behavior = Just bp } desc)

export
forCharacter : Vector2D -> Character -> Creation
forCharacter position character
  = MkCreation (ref character) position Nothing Nothing Nothing Nothing Nothing
               Nothing Nothing

public export
record Background where
  constructor MkBackground
  image : ContentReference
  dimensions : Vector2D

export
Show Background where
  show (MkBackground image dimensions) = image ++ " " ++ show dimensions

ObjectCaster Background where
  objectCast dict = with Checked do
    image <- getString "image" dict
    dimensions <- getVector "dimensions" dict
    pure $ MkBackground image dimensions

Serialize Background where
  toDict bg = with ST do
    bgObject <- makeObject
    addString bgObject "image" $ image bg
    addVector bgObject "dimensions" $ dimensions bg
    getDict bgObject

public export
record StaticCreation where
  constructor MkStaticCreation
  id : String
  creation : Creation

ObjectCaster StaticCreation where
  objectCast dict = with Checked do
    id <- getString "id" dict
    creation <- the (Checked Creation) $ objectCast dict
    pure $ MkStaticCreation id creation

Serialize StaticCreation where
  toDict sc = with ST do
    scObject <- makeObjectFrom $ creation sc
    addString scObject "id" $ id sc
    getDict scObject

export
processStaticCreations : List (StaticCreation, ObjectDescription) ->
                         List (Creation, ObjectDescription)
processStaticCreations = map processPair where
  processPair : (StaticCreation, ObjectDescription) -> (Creation, ObjectDescription)
  processPair (MkStaticCreation id creation, desc)
    = (((setId id) . makeCreationStatic) creation, makeObjectDescStatic desc)

public export
record MapDescription where
  constructor MkMapDescription
  name : String
  dimensions : Vector2D
  spawn : Vector2D
  background : Background
  creations : List Creation
  static : List StaticCreation
  joints : List JointDescription
  music : Maybe ContentReference

export
Show MapDescription where
  show (MkMapDescription name dimensions spawn background creations static joints music)
    =  "{ name: " ++ name
    ++ ", dimensions: " ++ show dimensions
    ++ ", spawn: " ++ show spawn
    ++ ", background: " ++ show background
    ++ ", creations: " ++ show creations
    ++ ", music: " ++ show music
    ++ " }"

export
ObjectCaster MapDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    dimensions <- getVector "dimensions" dict
    spawn <- getVector "spawn" dict
    background <- the (Checked Background) $ getCastable "background" dict
    creationsJSON <- getArray "creations" dict
    staticJSON <- getArray "static" dict
    jointsJSON <- getArray "joints" dict -- TODO maybe
    creations <- catResults $ the (List (Checked Creation)) $ map cast creationsJSON
    static <- catResults $ the (List (Checked StaticCreation)) $ map cast staticJSON
    joints <- catResults $ the (List (Checked JointDescription)) $ map cast jointsJSON
    music <- getStringMaybe "music" dict
    pure $ MkMapDescription name dimensions spawn background creations static joints music

export
Serialize MapDescription where
  toDict md = with ST do
    mdObject <- makeObject
    addString mdObject "name" $ name md
    addVector mdObject "dimensions" $ dimensions md
    addVector mdObject "spawn" $ spawn md
    addObject' mdObject "background" $ background md
    addObjectArray mdObject "creations" $ creations md
    addObjectArray mdObject "static" $ static md
    addObjectArray mdObject "joints" $ joints md
    addStringMaybe mdObject "music" $ music md
    getDict mdObject

export
addDynamic : Creation -> MapDescription -> MapDescription
addDynamic creation = record { creations $= append creation }

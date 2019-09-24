module Serializer

import public Control.ST
import public Control.ST.ImplicitCall
import public Control.Monad.Identity
import public Language.JSON
import Data.AVL.Dict
import Physics.Box2D

import Objects

public export
JSONDict : Type
JSONDict = Dict String JSON
%name JSONDict dict

public export
JSONList : Type
JSONList = List JSON
%name JSONList array

export
dictToJSON : JSONDict -> JSON
dictToJSON = JObject . toList

public export
data SerializerKind = JSONOBject | JSONArray

public export
interface Serializer (m : Type -> Type) where
  SKind : SerializerKind -> Type

  makeObject : ST m Var [add (SKind JSONOBject)]
  addNull : (object : Var) ->
            (key : String) ->
            ST m () [object ::: SKind JSONOBject]
  addBool : (object : Var) ->
            (key : String) ->
            (value : Bool) ->
            ST m () [object ::: SKind JSONOBject]
  addDouble : (object : Var) ->
              (key : String) ->
              (value : Double) ->
              ST m () [object ::: SKind JSONOBject]
  addString : (object : Var) ->
              (key : String) ->
              (value : String) ->
              ST m () [object ::: SKind JSONOBject]
  addArray : (object : Var) ->
             (key : String) ->
             (value : JSONList) ->
             ST m () [object ::: SKind JSONOBject]
  addObject : (object : Var) ->
              (key : String) ->
              (value : JSONDict) ->
              ST m () [object ::: SKind JSONOBject]
  getDict : (object : Var) -> ST m JSONDict [remove object (SKind JSONOBject)]

  makeArray : ST m Var [add (SKind JSONArray)]
  appendNull : (array : Var) -> ST m () [array ::: SKind JSONArray]
  appendBool : (array : Var) -> (value : Bool) -> ST m () [array ::: SKind JSONArray]
  appendDouble : (array : Var) -> (value : Double) -> ST m () [array ::: SKind JSONArray]
  appendString : (array : Var) -> (value : String) -> ST m () [array ::: SKind JSONArray]
  appendArray : (array : Var) -> (value : JSONList) -> ST m () [array ::: SKind JSONArray]
  appendObject : (array : Var) -> (value : JSONDict) -> ST m () [array ::: SKind JSONArray]
  getArray : (array : Var) -> ST m JSONList [remove array (SKind JSONArray)]

export
Serializer Identity where
  SKind JSONOBject = State JSONDict
  SKind JSONArray = State JSONList

  makeObject = new empty
  addNull object key = update object $ insert key JNull
  addDouble object key value = update object $ insert key (JNumber value)
  addString object key value = update object $ insert key (JString value)
  addBool object key value = update object $ insert key (JBoolean value)
  addArray object key value = update object $ insert key (JArray value)
  addObject object key value = update object $ insert key (dictToJSON value)
  getDict object = with ST do
    object' <- read object
    delete object
    pure object'

  makeArray = new empty
  appendNull array = update array $ append JNull
  appendBool array value = update array $ append (JBoolean value)
  appendDouble array value = update array $ append (JNumber value)
  appendString array value = update array $ append (JString value)
  appendArray array value = update array $ append (JArray value)
  appendObject array value = update array $ append (dictToJSON value)
  getArray array = with ST do
    array' <- read array
    delete array
    pure array'

export
addInt : (object : Var) ->
         (key : String) ->
         (value : Int) ->
         ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addInt object key x = addDouble object key (cast x)

export
addIntPair : (object : Var) ->
             (key : String) ->
             (value : (Int, Int)) ->
             ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addIntPair object key (x, y) = with ST do
  intArray <- makeArray
  appendDouble intArray $ cast x
  appendDouble intArray $ cast y
  intArray' <- getArray intArray
  addArray object key intArray'

export
addVector : (object : Var) ->
            (key : String) ->
            (value : Vector2D) ->
            ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addVector object key (x, y) = with ST do
  vectorArray <- makeArray
  appendDouble vectorArray x
  appendDouble vectorArray y
  vectorArray' <- getArray vectorArray
  addArray object key vectorArray'

export
addVectorMaybe : (object : Var) ->
                 (key : String) ->
                 (value : Maybe Vector2D) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addVectorMaybe object key (Just vec) = addVector object key vec
addVectorMaybe object key Nothing = pure ()

export
appendDoubles : (array : Var) ->
                (xs : List Double) ->
                ST Identity () [array ::: SKind JSONArray {m=Identity}]
appendDoubles array [] = pure ()
appendDoubles array (x::xs) = with ST do
  appendDouble array x
  appendDoubles array xs

export
addDoubleArray : (object : Var) ->
                 (key : String) ->
                 (xs : List Double) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addDoubleArray object key xs = with ST do
  doubleArray <- makeArray
  appendDoubles doubleArray xs
  doubleArray' <- getArray doubleArray
  addArray object key doubleArray'

export
appendVector : (array : Var) ->
               Vector2D ->
               ST Identity () [array ::: SKind JSONArray {m=Identity}]
appendVector array (x, y) = with ST do
  vectorArray <- makeArray
  appendDouble vectorArray x
  appendDouble vectorArray y
  vectorArray' <- getArray vectorArray
  appendArray array vectorArray'

export
appendVectors : (array : Var) ->
                (xs : List Vector2D) ->
                ST Identity () [array ::: SKind JSONArray {m=Identity}]
appendVectors array [] = pure ()
appendVectors array (x::xs) = with ST do
  appendVector array x
  appendVectors array xs

export
addVectorArray : (object : Var) ->
                 (key : String) ->
                 (value : List Vector2D) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addVectorArray object key xs = with ST do
  vectorArray <- makeArray
  appendVectors vectorArray xs
  vectorArray' <- getArray vectorArray
  addArray object key vectorArray'

export
appendStrings : (array : Var) ->
                (xs : List String) ->
                ST Identity () [array ::: SKind JSONArray {m=Identity}]
appendStrings array [] = pure ()
appendStrings array (x::xs) = with ST do
  appendString array x
  appendStrings array xs

export
addStringArray : (object : Var) ->
                 (key : String) ->
                 (value : List String) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addStringArray object key value = with ST do
  stringArray <- makeArray
  appendStrings stringArray value
  stringArray' <- getArray stringArray
  addArray object key stringArray'

export
addStringMaybe : (object : Var) ->
                 (key : String) ->
                 (value : Maybe String) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addStringMaybe object key (Just s) = addString object key s
addStringMaybe object key Nothing = pure ()

export
addDoubleMaybe : (object : Var) ->
                 (key : String) ->
                 (value : Maybe Double) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addDoubleMaybe object key (Just d) = addDouble object key d
addDoubleMaybe object key Nothing = pure ()

export
addIntMaybe : (object : Var) ->
              (key : String) ->
              (value : Maybe Int) ->
              ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addIntMaybe object key (Just i) = addInt object key i
addIntMaybe object key Nothing = pure ()

export
addBoolMaybe : (object : Var) ->
               (key : String) ->
               (value : Maybe Bool) ->
               ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addBoolMaybe object key (Just b) = addBool object key b
addBoolMaybe object key Nothing = pure ()

export
addObjectMaybe' : (object : Var) ->
                  (key : String) ->
                  (value : Maybe JSONDict) ->
                  ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addObjectMaybe' object key (Just dict) = addObject object key dict
addObjectMaybe' object key Nothing = pure ()

public export
interface Serialize a where
  toDict : a -> ST Identity JSONDict []

export
serialize : Serialize a => a -> JSONDict
serialize = runIdentity . run . toDict

export
serializeToJSON : Serialize a => a -> JSON
serializeToJSON = dictToJSON . serialize

export
addObjectMaybe : Serialize a =>
                 (object : Var) ->
                 (key : String) ->
                 (value : Maybe a) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addObjectMaybe object key = addObjectMaybe' object key . map serialize

export
addObject' : Serialize a =>
             (object : Var) ->
             (key : String) ->
             (value : a) ->
             ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addObject' object key value = addObject object key $ serialize value

export
pretty : Serialize a => a -> String
pretty = format 5 . serializeToJSON

export
makeDict : Serialize a => Dict String a -> JSONDict
makeDict = fromList . map serializeEntry . toList where
  serializeEntry : (String, a) -> (String, JSON)
  serializeEntry (key, value) = (key, serializeToJSON value)

export
appendObjects : Serialize a =>
                (array : Var) ->
                (xs : List a) ->
                ST Identity () [array ::: SKind JSONArray {m=Identity}]
appendObjects array [] = pure ()
appendObjects array (x::xs) = with ST do
  appendObject array $ serialize x
  appendObjects array xs

export
addObjectArray : Serialize a =>
                 (object : Var) ->
                 (key : String) ->
                 (value : List a) ->
                 ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addObjectArray object key value = with ST do
  objectArray <- makeArray
  appendObjects objectArray value
  objectArray' <- getArray objectArray
  addArray object key objectArray'

-- -- shouldn't be used
-- export
-- extendObject : Serialize a =>
--                (object : Var) ->
--                (key : String) ->
--                (value : a) ->
--                ST Identity () [object ::: SKind JSONOBject {m=Identity}]

export
makeObjectFrom : Serialize a => a -> ST Identity Var [add (SKind JSONOBject {m=Identity})]
makeObjectFrom = new . serialize

addDict : (cstr : a -> JSON) ->
          (object : Var) ->
          (key : String) ->
          (value : Dict String a) ->
          ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addDict cstr object key = addObject object key . map cstr

export
addIntDict : (object : Var) ->
             (key : String) ->
             (value : Dict String Int) ->
             ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addIntDict = addDict (JNumber . cast)

export
addDoubleDict : (object : Var) ->
                (key : String) ->
                (value : Dict String Double) ->
                ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addDoubleDict = addDict JNumber

export
addStringDict : (object : Var) ->
                (key : String) ->
                (value : Dict String String) ->
                ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addStringDict = addDict JString

export
addStringlistDict : (object : Var) ->
                    (key : String) ->
                    (value : Dict String (List String)) ->
                    ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addStringlistDict = addDict $ \xs => JArray $ map JString xs

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

public export
interface Serialize a where
  toDict : a -> ST Identity JSONDict []

export
serialize : Serialize a => a -> JSON
serialize = dictToJSON . (runIdentity . run) . toDict

export
pretty : Serialize a => a -> String
pretty = format 5 . serialize

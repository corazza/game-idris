module Objects

import public Data.AVL.DDict

public export
ObjectId : Type
ObjectId = String

public export
Objects : Type -> Type
Objects ty = DDict ObjectId ty

export
addObject : ObjectId -> ty -> Objects ty -> Objects ty
addObject sceneId x dict = case hasKey sceneId dict of
  False => insert sceneId x dict
  True => dict

export
removeObject : ObjectId -> Objects ty -> Objects ty
removeObject = delete

export
updateObject : (id : ObjectId) -> (f : ty -> ty) -> Objects ty -> Objects ty
updateObject = update

export
emptyObjects : Objects ty
emptyObjects = empty


public export
Ids : (ty : Type) -> Type
Ids ty = DDict ty ObjectId

export
addId : Ord ty => ty -> ObjectId -> Ids ty -> Ids ty
addId = insert

export
removeId : Ord ty => ty -> Ids ty -> Ids ty
removeId = delete

export
emptyIds : Ord a => Ids a
emptyIds = empty

module Scene.Objects

import public Data.AVL.DDict

public export
SceneId : Type
SceneId = String

public export
Objects : Type -> Type
Objects ty = DDict SceneId ty

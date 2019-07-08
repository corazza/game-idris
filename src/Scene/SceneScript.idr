module Scene.SceneScript

import Data.AVL.Set
import Data.AVL.DDict

import Common
import Physics.Vector2D
import Descriptors

-- doesn't know anything about Object, so can't have QueryObject
public export
data SceneScript : Type -> Type where
  GetPosition : (id : ObjectId) -> SceneScript (Maybe Vector2D)
  GetVelocity : (id : ObjectId) -> SceneScript (Maybe Vector2D)
  GetMass : (id : ObjectId) -> SceneScript (Maybe Double)

  Create : Creation -> SceneScript ()
  Destroy : (id : ObjectId) -> SceneScript ()
  Damage : Double -> (id : ObjectId) -> SceneScript ()
  DeactivateCollision : String -> ObjectId -> SceneScript ()

  Log : String -> SceneScript ()

  Pure : (res : a) -> SceneScript a
  (>>=) : SceneScript a -> (a -> SceneScript b) -> SceneScript b

export
Functor SceneScript where
  map f x = do res <- x
               Pure (f res)

export
Applicative SceneScript where
  pure = Pure
  sf <*> sa = do f <- sf
                 a <- sa
                 pure (f a)

export
Monad SceneScript where
  (>>=) = SceneScript.(>>=)

public export
UnitScript : Type
UnitScript = SceneScript ()

energy : CollisionData -> SceneScript (Maybe Double)
energy (MkCollisionData self other) = with SceneScript do
  Just mass <- GetMass (id self) | pure Nothing
  -- the object stopped a projectile relative to itself
  let v = magnitude ((velocity self) - (velocity other))
  pure $ Just (0.5*mass*v*v)

export
projectileDamage : (factor : Double) -> CollisionData -> UnitScript
projectileDamage factor cdata@(MkCollisionData self other) = with SceneScript do
  Just energy <- energy cdata | pure ()
  Damage (factor * energy) (id other)
  -- Destroy (id self) -- TODO conditional on damage?
  DeactivateCollision "projectile" (id self)

-- TODO exports are wrong
public export
throw : (ref : ResourceReference) -> Double -> ActionParameters -> UnitScript
throw ref impulse (MkActionParameters id actionPosition) = with SceneScript do
  Just position <- GetPosition id | pure ()
  let direction = normed (actionPosition - position)
  let cdata = BoxData (Just $ impulse `scale` direction)
  Create $ MkCreation Nothing ref (position + (2 `scale` direction))
                      (angle direction - pi/2.0) empty cdata

public export
ScriptType : ScriptDescriptor -> Type
ScriptType (Create ref impulse) = ActionParameters -> UnitScript
ScriptType _ = UnitScript

public export
fromDescriptor : (desc : ScriptDescriptor) -> ScriptType desc
fromDescriptor (Create ref impulse) = throw ref impulse

export -- tags are recreated because other information from descriptor and creation might be relevant
decideCollisions : ObjectDescriptor -> Creation -> DDict String (CollisionData -> UnitScript)
decideCollisions desc creation = let object_tags = (tags desc) `union` (tags creation) in
  if contains Projectile object_tags
    then insert "projectile" (projectileDamage 0.5) empty
    else empty

export
decideAttack : ObjectDescriptor -> Maybe (ActionParameters -> UnitScript)
decideAttack desc = with Maybe do
  attackDescriptor <- attack desc
  -- Just attackDescriptor@(Create x) => Just $ fromDescriptor attackDescriptor
  case attackDescriptor of
    desc@(Create ref impulse) => pure $ fromDescriptor desc
    _ => Nothing

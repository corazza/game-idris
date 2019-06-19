module Script

import Data.AVL.Set
import Data.AVL.Dict

import Common
import Physics.Vector2D
import Descriptors

-- doesn't know anything about Object, so can't have QueryObject
public export
data Script : Type -> Type where
  GetPosition : (id : ObjectId) -> Script (Maybe Vector2D)
  GetVelocity : (id : ObjectId) -> Script (Maybe Vector2D)
  GetMass : (id : ObjectId) -> Script (Maybe Double)

  Create : Creation -> Script ()
  Destroy : (id : ObjectId) -> Script ()
  Damage : Double -> (id : ObjectId) -> Script ()
  DeactivateCollision : String -> ObjectId -> Script ()

  Print : String -> Script ()

  Pure : (res : a) -> Script a
  (>>=) : Script a -> (a -> Script b) -> Script b

export
Functor Script where
  map f x = do res <- x
               Pure (f res)

export
Applicative Script where
  pure = Pure
  sf <*> sa = do f <- sf
                 a <- sa
                 pure (f a)

export
Monad Script where
  (>>=) = Script.(>>=)

public export
UnitScript : Type
UnitScript = Script ()

-- this exists only to introduce ordering to collision events (needed by scripts)
public export
record CollisionData where
  constructor MkCollisionData
  self : CollisionForObject
  other : CollisionForObject

public export
data Selector = First | Second

export
buildCollisionData : CollisionForObject -> CollisionForObject -> Selector -> CollisionData
buildCollisionData one two First = MkCollisionData one two
buildCollisionData one two Second = MkCollisionData two one

energy : CollisionData -> Script (Maybe Double)
energy (MkCollisionData self other) = with Script do
  Just mass <- GetMass (id self) | pure Nothing
  -- the object stopped a projectile relative to itself
  let v = magnitude ((velocity self) - (velocity other))
  pure $ Just (0.5*mass*v*v)

export
projectileDamage : (factor : Double) -> CollisionData -> UnitScript
projectileDamage factor cdata@(MkCollisionData self other) = with Script do
  Just energy <- energy cdata | pure ()
  Damage (factor * energy) (id other)
  -- Destroy (id self) -- TODO conditional on damage?
  DeactivateCollision "projectile" (id self)

-- TODO exports are wrong
public export
throw : (ref : ResourceReference) -> ActionParameters -> UnitScript
throw ref (MkActionParameters id actionPosition impulse) = with Script do
  Just position <- GetPosition id | pure ()
  let direction = normed (actionPosition - position)
  let impulse' = getOrDefault 0 impulse
  let cdata = BoxData (Just $ impulse' `scale` direction)
  Create $ MkCreation Nothing ref (position + (2 `scale` direction))
                      (angle direction - pi/2.0) empty cdata

public export
ScriptType : ScriptDescriptor -> Type
ScriptType (Create ref) = ActionParameters -> UnitScript
ScriptType _ = UnitScript

public export
fromDescriptor : (desc : ScriptDescriptor) -> ScriptType desc
fromDescriptor (Create ref) = throw ref

export -- tags are recreated because other information from descriptor and creation might be relevant
decideCollisions : ObjectDescriptor -> Creation -> Dict String (Maybe (CollisionData -> UnitScript))
decideCollisions desc creation = let object_tags = (tags desc) `union` (tags creation) in
  if contains Projectile object_tags
    then insert "projectile" (Just (projectileDamage 0.5)) empty
    else empty

export
decideAttack : ObjectDescriptor -> Maybe (ActionParameters -> UnitScript)
decideAttack desc = with Maybe do
  attackDescriptor <- attack desc
  -- Just attackDescriptor@(Create x) => Just $ fromDescriptor attackDescriptor
  case attackDescriptor of
    Create x => pure $ fromDescriptor (Create x)
    _ => Nothing

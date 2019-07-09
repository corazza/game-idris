module Scene.SceneScript

import Data.AVL.Set
import Data.AVL.DDict

import Common
import Events
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
  ProduceEvent : Events.Event -> SceneScript ()

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

emptyAction : ActionParameters -> UnitScript
emptyAction = const $ pure ()

energy : CollisionData -> SceneScript (Maybe Double)
energy (MkCollisionData self other) = with SceneScript do
  Just mass <- GetMass (id self) | pure Nothing
  -- the object stopped a projectile relative to itself
  let v = magnitude ((velocity self) - (velocity other))
  pure $ Just (0.5*mass*v*v)

hit : (attacker : ObjectId) -> (target : ObjectId) -> (damage : Double) -> UnitScript
hit attacker target damage = with SceneScript do
  Damage damage target
  ProduceEvent $ Hit attacker target damage

export
projectileDamage : (factor : Double) -> (creator : ObjectId) -> CollisionData -> UnitScript
projectileDamage factor creator cdata@(MkCollisionData self other) = with SceneScript do
  Just energy <- energy cdata | pure ()
  hit creator (id other) (factor * energy)
  DeactivateCollision "projectile" (id self)

-- TODO exports are wrong
export
throw : (thrower : ObjectId) -> (ref : ResourceReference) -> Double -> ActionParameters -> UnitScript
throw thrower ref impulse (MkActionParameters id actionPosition) = with SceneScript do
  Just position <- GetPosition id | pure ()
  let direction = normed (actionPosition - position)
  let cdata = BoxData (Just $ impulse `scale` direction)
  Create $ MkCreation Nothing ref (position + (2 `scale` direction))
                      (angle direction - pi/2.0) empty cdata (Just thrower)

export
ScriptType : ScriptDescriptor -> Type
ScriptType (Create ref impulse) = ActionParameters -> UnitScript
ScriptType _ = UnitScript

export
scriptFromDescriptor : (creation : Creation) -> (desc : ScriptDescriptor) -> ScriptType desc
scriptFromDescriptor creation (Create ref impulse) = case Creation.id creation of
  Nothing => emptyAction
  Just thrower => throw thrower ref impulse

export -- tags are recreated because other information from descriptor and creation might be relevant
decideCollisions : ObjectDescriptor -> Creation -> DDict String (CollisionData -> UnitScript)
decideCollisions desc creation = let object_tags = (tags desc) `union` (tags creation) in
  if contains Projectile object_tags
    then case creator creation of
      Nothing => empty
      Just creator_id => insert "projectile" (projectileDamage 0.5 creator_id) empty
    else empty

export
decideAttack : ObjectDescriptor -> Creation -> Maybe (ActionParameters -> UnitScript)
decideAttack desc creation = with Maybe do
  attackDescriptor <- attack desc
  -- Just attackDescriptor@(Create x) => Just $ fromDescriptor attackDescriptor
  case attackDescriptor of
    desc@(Create ref impulse) => pure $ scriptFromDescriptor creation desc
    _ => Nothing

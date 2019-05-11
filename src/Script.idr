module Script

import Common
import Physics.Vector2D

-- HERE now Box2D events have to be polled

public export
data Script : Type -> Type where
  Damage : Double -> (id : ObjectId) -> Script ()
  GetVelocity : (id : ObjectId) -> Script (Maybe Vector2D)
  GetMass : (id : ObjectId) -> Script (Maybe Double)
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

export
UnitScript : Type
UnitScript = Script ()

energy : ObjectId -> Script (Maybe Double)
energy id = with Script do
  Just velocity <- GetVelocity id | pure Nothing
  Just mass <- GetMass id | pure Nothing
  pure $ Just (0.5*mass*(Doubles.pow (magnitude velocity) 2))

public export
record CollisionData where
  constructor MkCollisionData
  self : ObjectId
  other : ObjectId

projectileDamage : (factor : Double) -> CollisionData -> Script ()
projectileDamage factor (MkCollisionData self other) = with Script do
  Just energy <- energy self | pure ()
  Damage (factor * energy) other

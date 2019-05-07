module Script

import Objects
import Physics.Vector2D

public export
data Script : a -> Type where
  Damage : Double -> ObjectId -> Script ()
  GetVelocity : ObjectId -> Script Vector2D
  GetMass : ObjectId -> Script Double

  Pure : (res : a) -> Script a
  (>>=) : Script a -> (a -> Script b) -> Script b

Functor Script where
  map f x = do res <- x
               Pure (f res)

Applicative Script where
  pure = Pure
  sf <*> sa = do f <- sf
                 a <- sa
                 pure (f a)

Monad Script where
  (>>=) = Script.(>>=)

energy : ObjectId -> Script Double
energy id = with Script do
  let velocity = magnitude !(GetVelocity id)
  mass <- GetMass id
  pure (0.5*mass*(Doubles.pow velocity 2))

public export
record CollisionData where
  constructor MkCollisionData
  self : ObjectId
  other : ObjectId

projectileDamage : (factor : Double) -> CollisionData -> Script ()
projectileDamage factor (MkCollisionData self other) =
  Damage (factor * !(energy self)) other

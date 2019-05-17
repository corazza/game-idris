module Common

import Physics.Vector2D
import Data.AVL.Dict
import Data.AVL.Set

public export
data Color = MkColor Int Int Int Int

public export
ResourceReference : Type
ResourceReference = String
%name ResourceReference ref

public export
ObjectId : Type
ObjectId = String
%name ObjectId id

public export
record CollisionForObject where
  constructor MkCollisionForObject
  id : ObjectId
  velocity : Vector2D

public export
record ActionParameters where
  constructor MkActionParameters
  id : ObjectId
  position : Vector2D
  impulse : Maybe Double

-- TODO shouldn't be here, but in Objects
public export
data ObjectTag = Spawn | Projectile

export
Show ObjectTag where
  show Spawn = "spawn"
  show Projectile = "projectile"

export
Eq ObjectTag where
  Spawn == Spawn = True
  Projectile == Projectile = True
  _ == _ = False

export
Ord ObjectTag where
  compare a b = compare (show a) (show b)

export
round : Double -> Int
round x = if x < 0.0 then cast (x - 0.5) else cast (x + 0.5)

public export
Cast (Int, Int) (Double, Double) where
  cast (x, y) = (cast x, cast y)

public export
getOrDefault : a -> (Maybe a) -> a
getOrDefault x Nothing = x
getOrDefault x (Just y) = y

-- -- TODO WARNING removal super-inefficient
-- export
-- delete : Ord k => (key : k) -> (dict : Dict k v) -> Dict k v
-- delete key dict = let asList = toList dict
--                       result = filter (\(k', v')=>k' /= key) asList in fromList result

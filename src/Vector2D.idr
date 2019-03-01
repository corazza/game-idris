module Vector2D

public export
Vector2D : Type
Vector2D = (Double, Double)

export
Num Vector2D where
  (a1, b1) + (a2, b2) = (a1+a2, b1+b2)
  (a1, b1) * (a2, b2) = (a1*a2, b1*b2)
  fromInteger a = (cast a, cast a)

export
scale : Double -> Vector2D -> Vector2D
scale a (x, y) = (a*x, a*y)

public export
Cast (Integer, Integer) Vector2D where
  cast (x, y) = (the Double (cast x), the Double (cast y))

public export
Cast Vector2D (Int, Int) where
  cast (x, y) = (cast x, cast y)

export
nullVector : Vector2D
nullVector = (0, 0)

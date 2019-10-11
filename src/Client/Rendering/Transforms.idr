module Client.Rendering.Transforms

import Client.Rendering.PositionData

import Physics.Vector2D

Operator : Type
Operator = ((Double, Double), (Double, Double))

applyOperator : Operator -> Vector2D -> Vector2D
applyOperator ((a, c), (b, d)) (x, y) = (a*x + c*y, b*x + d*y)

rotationMatrix : Double -> Operator
rotationMatrix angle' = let angle = degToRad angle'
                            in ((cos angle, -(sin angle)), (sin angle, cos angle))

rotate : Double -> Vector2D -> Vector2D
rotate angle vec = applyOperator (rotationMatrix angle) vec

export
rotatePoint : (angle : Double) ->
              (around : Vector2D) ->
              (point : Vector2D) ->
              Vector2D
rotatePoint angle around point = (rotate angle (point - around)) + around

export
rotatePoints : (angle : Double) ->
               (around : Vector2D) ->
               (points : List Vector2D) ->
               List Vector2D
rotatePoints angle around = map $ rotatePoint angle around

export
makeRectPoints : (position : Vector2D) -> (dims : Vector2D) -> List Vector2D
makeRectPoints (x, y) (w, h)
  = [(x-w, y+h), (x+w, y+h), (x+w, y-h), (x-w, y-h)]

export
makeRotatedRect : (angle : Double) ->
                  (position : Vector2D) ->
                  (dims : Vector2D) ->
                  List Vector2D
makeRotatedRect angle position dims
  = rotatePoints angle position $ makeRectPoints position dims

minmax : List Double -> (Double, Double)
minmax xs
  = let xs_ascending = sort xs
        xs_descending = sortBy (flip compare) xs
        minx = fromMaybe 0 $ head' xs_ascending
        maxx = fromMaybe 0 $ head' xs_descending
        in (minx, maxx)

export
getAABB : List Vector2D -> (Vector2D, Vector2D)
getAABB vecs
  = let n = the Int $ cast $ length vecs
        xs = map fst vecs
        ys = map snd vecs
        (minx, maxx) = minmax xs
        (miny, maxy) = minmax ys
        w = (maxx - minx)/2
        h = (maxy - miny)/2
        x = (sum xs) / (cast n)
        y = (sum ys) / (cast n)
        in ((x, y), (w, h))

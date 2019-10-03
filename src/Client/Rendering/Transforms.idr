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

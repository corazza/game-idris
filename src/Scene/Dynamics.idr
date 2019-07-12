module Scene.Dynamics

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D
import Physics.Vector2D

import Scene.Dynamics.PDynamics
import Scene.Dynamics.Control
import Scene.Objects
import Exception

public export
interface Dynamics (m : Type -> Type) where
  SDynamics : Type

  startDynamics : ST m Var [add SDynamics]
  endDynamics : (dynamics : Var) -> ST m () [remove dynamics SDynamics]

  addBody : (dynamics : Var) ->
            (id : SceneId) ->
            (def : BodyDefinition) ->
            (fixtures : List FixtureDefinition) ->
            (control : Maybe ControlParameters) ->
            ST m () [dynamics ::: SDynamics]

  removeBody : (dynamics : Var) -> (id : SceneId) -> ST m () [dynamics ::: SDynamics]

  iterate : (dynamics : Var) -> (ticks : Int) -> ST m () [dynamics ::: SDynamics]

  updateControl : (dynamics : Var) ->
                  (id : SceneId) ->
                  (f : ControlState -> ControlState) ->
                  ST m () [dynamics ::: SDynamics]

  getPosition : (dynamics : Var) -> (id : SceneId) -> ST m (Checked Vector2D) [dynamics ::: SDynamics]
  getAngle : (dynamics : Var) -> (id : SceneId) -> ST m (Checked Double) [dynamics ::: SDynamics]

export
Dynamics IO where
  SDynamics = Int

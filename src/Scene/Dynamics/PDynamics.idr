module Scene.Dynamics.PDynamics

import Scene.Dynamics.Control

import Physics.Box2D
import Objects

public export
record PDynamics where
  constructor MkPDynamics
  world : Box2D.World
  controls : Objects (ControlState, ControlParameters)
  bodies : Objects (Body, Int)

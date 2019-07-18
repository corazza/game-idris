module Server.Rules

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Server.Rules.PRules

public export
interface Rules (m : Type -> Type) where
  SRules : Type

  startRules : ST m Var [add SRules]

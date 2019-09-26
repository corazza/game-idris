module Server.Rules.RuleScript

import Physics.Box2D

import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Server.Rules.NumericProperties
import Server.Rules.RulesData
import Client.ClientCommands
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.BodyFlags
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Descriptions.ItemDescription
import Descriptions.MapDescription
import Descriptions.AbilityDescription
import Dynamics.DynamicsEvent
import Dynamics.BodyData
import Dynamics.MoveDirection
import Commands
import GameIO
import Objects
import Exception
import Timeline
import Timeline.Items

public export
data RuleScript : Type -> Type where
  Output : RulesOutput -> RuleScript ()
  Transition : (id : ObjectId) -> BehaviorState -> List (RuleScript ()) -> RuleScript ()
  UpdateBehaviorData : (id : ObjectId) -> (f : BehaviorData -> BehaviorData) -> RuleScript ()
  QueryBehaviorData : (id : ObjectId) -> (q : BehaviorData -> a) -> RuleScript (Maybe a)
  RulesClientCommand : (id : ObjectId) -> ClientCommand -> RuleScript ()

  GetItemDescription : (ref : ContentReference) -> RuleScript (Checked ItemDescription)
  GetAttack : (id : ObjectId) -> RuleScript (Maybe ContentReference)
  GetBody : (id : ObjectId) -> RuleScript (Maybe BodyData)
  QueryBody : (id : ObjectId) -> (q : BodyData -> a) -> RuleScript (Maybe a)

  GetStartTime : (id : ObjectId) -> RuleScript (Maybe Int) -- time since in this state
  GetDirection : (id : ObjectId) -> RuleScript (Maybe BehaviorDirection)
  GetController : (id : ObjectId) -> RuleScript (Maybe BehaviorController)
  GetPosition : (id : ObjectId) -> RuleScript (Maybe Vector2D)

  QueryData : (id : ObjectId) -> (q : RulesData -> a) -> RuleScript (Maybe a)
  UpdateData : (id : ObjectId) -> (f : RulesData -> RulesData) -> RuleScript ()

  QueryItems : (id : ObjectId) -> (q : Items -> a) -> RuleScript (Maybe a)
  UpdateItems : (id : ObjectId) -> (f : Items -> Items) -> RuleScript ()

  UpdateNumericProperty : ObjectId ->
                          NumericPropertyId ->
                          (f : NumericProperty -> NumericProperty) ->
                          RuleScript ()
  QueryNumericProperty : ObjectId ->
                         NumericPropertyId ->
                         (f : NumericProperty -> a) ->
                         RuleScript (Maybe a)

  GetStat : (for : ObjectId) -> StatId -> RuleScript (Maybe Double)
  GetCreator : ObjectId -> RuleScript (Maybe ObjectId)

  GetTime : RuleScript Int -- global time

  Log : String -> RuleScript ()

  Pure : (res : a) -> RuleScript a
  (>>=) : RuleScript a -> (a -> RuleScript b) -> RuleScript b

export
Functor RuleScript where
  map f x = do res <- x
               Pure (f res)

export
Applicative RuleScript where
  pure = Pure
  sf <*> sa = do f <- sf
                 a <- sa
                 pure (f a)

export
Monad RuleScript where
  (>>=) = RuleScript.(>>=)

public export
UnitRuleScript : Type
UnitRuleScript = RuleScript ()

export
scriptFilter : (f : a -> RuleScript Bool) -> List a -> RuleScript (List a)
scriptFilter f [] = pure []
scriptFilter f (x :: xs) = case !(f x) of
  False => scriptFilter f xs
  True => pure $ x :: !(scriptFilter f xs)

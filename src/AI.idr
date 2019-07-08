module AI

import Control.ST
import Control.ST.ImplicitCall

import AI.AIScript
import AI.Controller
import AI.AIEvents
import Data.AVL.DDict
import Descriptors
import Objects
import Events
import Common
import GameIO
import Resources

public export
interface AI (m : Type -> Type) where
  SAI : Type

  startAI : ST m Var [add SAI]
  endAI : (ai : Var) -> ST m () [remove ai SAI]

  addController : (ai : Var) -> (id : ObjectId) ->
                  (ref : ResourceReference) -> ST m () [ai ::: SAI]

  handle : (ai : Var) -> AIEvents.Event -> ST m () [ai ::: SAI]
  handleEvents : (ai : Var) -> List AIEvents.Event -> ST m () [ai ::: SAI]

  decisions : (ai : Var) -> (time : Int) -> ST m (List Events.Event) [ai ::: SAI]

  private
  passed : (ai : Var) -> (time : Int) -> ST m () [ai ::: SAI]

  private
  runAIScript : (ai : Var) -> (script : AIScript a) -> ST m a [ai ::: SAI]
  private
  runUnitScripts : (ai : Var) -> (scripts : List UnitAIScript) -> ST m () [ai ::: SAI]
  private
  runScripts : (ai : Var) -> ST m () [ai ::: SAI]

  private
  returnEvents : (ai : Var) -> ST m (List Events.Event) [ai ::: SAI]

AIControllers : Type
AIControllers = DDict ObjectId AIController

export
(ConsoleIO m, GameIO m) => AI m where
  SAI = Composite [State AIControllers,
                   State (List Events.Event),
                   State Int,
                   SCache {m} {r=AIDescriptor}]

  startAI = with ST do
    controllers <- new empty
    events <- new empty
    time <- new 0
    ai <- new ()
    cache <- initCache {r=AIDescriptor}
    combine ai [controllers, events, time, cache]
    pure ai

  endAI ai = with ST do
    [controllers, events, time, cache] <- split ai
    delete controllers; delete events; delete time
    quitCache {r=AIDescriptor} cache
    delete ai

  addController ai id ref = with ST do
    [controllers, events, time, cache] <- split ai
    Right desc <- get {m} {r=AIDescriptor} cache ref
               | Left e => with ST do
                    combine ai [controllers, events, time, cache]
                    putStrLn $ "couldn't get AI descriptor of " ++ ref ++ ", error: "
                    putStrLn e
    update controllers (insert id (fromDescriptor desc))
    combine ai [controllers, events, time, cache]

  handle ai event = pure ()

  handleEvents ai [] = pure ()
  handleEvents ai (x :: xs) = handle ai x >>= const (handleEvents ai xs)

  decisions ai dt = with ST do
    runScripts ai
    passed ai dt
    returnEvents ai

  passed ai dt = with ST do
    [controllers, events, time, cache] <- split ai
    update time (+ dt)
    combine ai [controllers, events, time, cache]

  runScripts ai = with ST do
    [controllers, events, time, cache] <- split ai
    controllers' <- read controllers
    combine ai [controllers, events, time, cache]
    runUnitScripts ai (map (uncurry produceScript) . toList $ controllers')

  returnEvents ai = with ST do
    [controllers, events, time, cache] <- split ai
    events' <- read events
    combine ai [controllers, events, time, cache]
    pure events'

  runAIScript ai (Event event) = with ST do
    [controllers, events, time, cache] <- split ai
    update events (event ::)
    combine ai [controllers, events, time, cache]
  runAIScript ai (UpdateData id f) = with ST do
    [controllers, events, time, cache] <- split ai
    update controllers (DDict.update id (updateData f))
    combine ai [controllers, events, time, cache]
  runAIScript ai (GetTime id) = with ST do
    [controllers, events, time, cache] <- split ai
    time' <- read time
    combine ai [controllers, events, time, cache]
    pure time'
  runAIScript ai (Log x) = lift $ GameIO.log x
  runAIScript ai (Pure res) = pure res
  runAIScript ai (x >>= f) = runAIScript ai x >>= (runAIScript ai) . f

  runUnitScripts ai [] = pure ()
  runUnitScripts ai (x :: xs) = with ST do
    runAIScript ai x
    runUnitScripts ai xs

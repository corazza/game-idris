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
Commands : Type
Commands = Dict ObjectId (List Command)

removeCommands : Commands -> Commands
removeCommands = map (const empty)

-- TODO Scene and AI use a similar pattern of a stateful store--MAYBE abstract?

public export
interface AI (m : Type -> Type) where
  SAI : Type

  startAI : ST m Var [add SAI]
  endAI : (ai : Var) -> ST m () [remove ai SAI]

  addController : (ai : Var) -> (id : ObjectId) ->
                  (ref : ResourceReference) -> ST m () [ai ::: SAI]
--
  removeController : (ai : Var) -> (id : ObjectId) -> ST m () [ai ::: SAI]


  handle : (ai : Var) -> Events.Event -> ST m () [ai ::: SAI]
  handleEvents : (ai : Var) -> List Events.Event -> ST m () [ai ::: SAI]

  decisions : (ai : Var) -> (time : Int) -> ST m Commands [ai ::: SAI]

  private
  getController : (ai : Var) -> (id : ObjectId) -> ST m (Maybe AIController) [ai ::: SAI]

  private
  queryController : (ai : Var) -> (id : ObjectId) -> (q : AIController -> a) -> ST m (Maybe a) [ai ::: SAI]

  private
  passed : (ai : Var) -> (time : Int) -> ST m () [ai ::: SAI]

  private
  runAIScript : (ai : Var) -> (script : AIScript a) -> ST m a [ai ::: SAI]
  private
  runUnitScripts : (ai : Var) -> (scripts : List UnitAIScript) -> ST m () [ai ::: SAI]
  private
  runScripts : (ai : Var) -> ST m () [ai ::: SAI]

  private
  returnCommands : (ai : Var) -> ST m Commands [ai ::: SAI]

AIControllers : Type
AIControllers = DDict ObjectId AIController

export
(ConsoleIO m, GameIO m) => AI m where
  SAI = Composite [State AIControllers,
                   State Commands,
                   State Int,
                   SCache {m} {r=AIDescriptor}]

  startAI = with ST do
    controllers <- new empty
    commands <- new empty
    time <- new 0
    ai <- new ()
    cache <- initCache {r=AIDescriptor}
    combine ai [controllers, commands, time, cache]
    pure ai

  endAI ai = with ST do
    [controllers, commands, time, cache] <- split ai
    delete controllers; delete commands; delete time
    quitCache {r=AIDescriptor} cache
    delete ai

  addController ai id ref = with ST do
    [controllers, commands, time, cache] <- split ai
    Right desc <- get {m} {r=AIDescriptor} cache ref
               | Left e => with ST do
                    combine ai [controllers, commands, time, cache]
                    putStrLn $ "couldn't get AI descriptor of " ++ ref ++ ", error: "
                    putStrLn e
    update controllers (insert id (fromDescriptor desc))
    update commands (insert id empty)
    combine ai [controllers, commands, time, cache]

  removeController ai id = with ST do
    [controllers, commands, time, cache] <- split ai
    combine ai [controllers, commands, time, cache]

  handle ai event = pure ()

  handleEvents ai [] = pure ()
  handleEvents ai (x :: xs) = handle ai x >>= const (handleEvents ai xs)

  decisions ai dt = with ST do
    runScripts ai
    passed ai dt
    returnCommands ai

  getController ai id = with ST do
    [controllers, commands, time, cache] <- split ai
    controllers' <- read controllers
    combine ai [controllers, commands, time, cache]
    pure $ lookup id controllers'

  queryController ai id q = pure $ map q !(getController ai id)

  passed ai dt = with ST do
    [controllers, commands, time, cache] <- split ai
    update time (+ dt)
    combine ai [controllers, commands, time, cache]

  runScripts ai = with ST do
    [controllers, commands, time, cache] <- split ai
    controllers' <- read controllers
    time' <- read time
    combine ai [controllers, commands, time, cache]
    let timeScripts' = uncurry (timeScripts time')
    runUnitScripts ai (map timeScripts' . toList $ controllers')

  returnCommands ai = with ST do
    [controllers, commands, time, cache] <- split ai
    commands' <- read commands
    update commands removeCommands
    combine ai [controllers, commands, time, cache]
    pure commands'

  runAIScript ai (AICommand id command) = with ST do
    [controllers, commands, time, cache] <- split ai
    update commands (Dict.update id (command ::))
    update controllers $ map $ updateData $ commandToDataUpdate command
    combine ai [controllers, commands, time, cache]
  runAIScript ai (UpdateData id f) = with ST do
    [controllers, commands, time, cache] <- split ai
    update controllers (DDict.update id (updateData f))
    combine ai [controllers, commands, time, cache]
  runAIScript ai (Transition id state action) = with ST do
    [controllers, commands, time, cache] <- split ai
    time' <- read time
    update controllers (DDict.update id (transition time' state))
    combine ai [controllers, commands, time, cache]
    case action of
      Nothing => pure ()
      Just x => runAIScript ai (actionToScript id x)
  runAIScript ai GetTime = with ST do
    [controllers, commands, time, cache] <- split ai
    time' <- read time
    combine ai [controllers, commands, time, cache]
    pure time'
  runAIScript ai (GetDirection id) = queryController ai id direction >>= pure . join
  runAIScript ai (GetStartTime id) = queryController ai id transitioned
  runAIScript ai (Log x) = lift $ GameIO.log x
  runAIScript ai (Pure res) = pure res
  runAIScript ai (x >>= f) = runAIScript ai x >>= (runAIScript ai) . f

  runUnitScripts ai [] = pure ()
  runUnitScripts ai (x :: xs) = runAIScript ai x >>= const (runUnitScripts ai xs)

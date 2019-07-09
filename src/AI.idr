module AI

import Control.ST
import Control.ST.ImplicitCall

import AI.AIScript
import AI.Controller
import AI.AIEvents
import AI.PAI
import Data.AVL.DDict
import Descriptors
import Objects
import Events
import Common
import GameIO
import Physics.Vector2D
import Resources

-- TODO Scene and AI use a similar pattern of a stateful store--MAYBE abstract?
public export
interface AI (m : Type -> Type) where
  SAI : Type

  startAI : ST m Var [add SAI]
  endAI : (ai : Var) -> ST m () [remove ai SAI]

  addController : (ai : Var) -> (id : ObjectId) ->
                  (ref : ResourceReference) -> ST m () [ai ::: SAI]
  removeController : (ai : Var) -> (id : ObjectId) -> ST m () [ai ::: SAI]


  handle : (ai : Var) -> Events.Event -> ST m () [ai ::: SAI]
  handleEvents : (ai : Var) -> List Events.Event -> ST m () [ai ::: SAI]

  relevantObjects : (ai : Var) -> ST m (List ObjectId) [ai ::: SAI]
  decisions : (ai : Var) -> (dt : Int) -> (objectUpdates : List ObjectInfo) -> ST m Commands [ai ::: SAI]

  private
  getController : (ai : Var) -> (id : ObjectId) -> ST m (Maybe AIController) [ai ::: SAI]
  private
  queryController : (ai : Var) -> (id : ObjectId) -> (q : AIController -> a) -> ST m (Maybe a) [ai ::: SAI]

  private
  updateObjectsInfo : (ai : Var) -> (info : List ObjectInfo) -> ST m () [ai ::: SAI]
  private
  passed : (ai : Var) -> (time : Int) -> ST m () [ai ::: SAI]


  private
  runAIScript : (ai : Var) -> (script : AIScript a) -> ST m a [ai ::: SAI]
  private
  runUnitScripts : (ai : Var) -> (scripts : List UnitAIScript) -> ST m () [ai ::: SAI]
  private
  mainScripts : (ai : Var) -> ST m () [ai ::: SAI]

  private
  returnCommands : (ai : Var) -> ST m Commands [ai ::: SAI]

export
(ConsoleIO m, GameIO m) => AI m where
  SAI = Composite [State PAI, SCache {m} {r=AIDescriptor}]

  startAI = with ST do
    pai <- new emptyPAI
    cache <- initCache {r=AIDescriptor}
    ai <- new ()
    combine ai [pai, cache]
    pure ai

  endAI ai = with ST do
    [pai, cache] <- split ai
    delete pai
    quitCache {r=AIDescriptor} cache
    delete ai

  addController ai id ref = with ST do
    [pai, cache] <- split ai
    Right desc <- get {m} {r=AIDescriptor} cache ref
               | Left e => with ST do
                    combine ai [pai, cache]
                    putStrLn $ "couldn't get AI descriptor of " ++ ref ++ ", error: "
                    putStrLn e
    update pai $ addControllerPAI (fromDescriptor desc) id
    update pai $ emptyCommandsFor id
    update pai $ addInfo id
    combine ai [pai, cache]

  removeController ai id = with ST do
    [pai, cache] <- split ai
    update pai $ removeControllerPAI id
    update pai $ removeInfo id
    combine ai [pai, cache]

  relevantObjects ai = with ST do
    [pai, cache] <- split ai
    let relevant = getRelevantObjects !(read pai)
    combine ai [pai, cache]
    pure relevant

  decisions ai dt objectUpdates = with ST do
    updateObjectsInfo ai objectUpdates
    mainScripts ai
    passed ai dt
    returnCommands ai

  getController ai id = with ST do
    [pai, cache] <- split ai
    pai' <- read pai
    combine ai [pai, cache]
    pure $ getControllerPAI id pai'

  queryController ai id q = pure $ map q !(getController ai id)

  updateObjectsInfo ai objectUpdates = with ST do
    [pai, cache] <- split ai
    update pai $ updateObjectsInfoPAI objectUpdates
    combine ai [pai, cache]

  passed ai dt = with ST do
    [pai, cache] <- split ai
    update pai (passedPAI dt)
    combine ai [pai, cache]

  mainScripts ai = with ST do
    [pai, cache] <- split ai
    pai' <- read pai
    combine ai [pai, cache]
    let mainScript' = uncurry $ mainScript $ time pai'
    runUnitScripts ai (map mainScript' . toList $ controllers pai')

  handle ai event = runAIScript ai (eventScript event)
  handleEvents ai [] = pure ()
  handleEvents ai (x :: xs) = handle ai x >>= const (handleEvents ai xs)

  returnCommands ai = with ST do
    [pai, cache] <- split ai
    let commands = commands !(read pai)
    update pai removeCommandsPAI
    combine ai [pai, cache]
    pure commands

  runAIScript ai (AICommand id command) = with ST do
    [pai, cache] <- split ai
    update pai (addCommandPAI id command)
    update pai $ updateController id $ updateData $ commandToDataUpdate command
    combine ai [pai, cache]
  runAIScript ai (UpdateData id f) = with ST do
    [pai, cache] <- split ai
    update pai $ updateController id $ updateData f
    combine ai [pai, cache]
  runAIScript ai (Transition id state action) = with ST do
    lift $ GameIO.log $ id ++ " transitioning to " ++ show state
    [pai, cache] <- split ai
    let time' = time !(read pai)
    update pai $ updateController id $ transition time' state
    combine ai [pai, cache]
    case action of
      Nothing => pure ()
      Just x => runAIScript ai (actionToScript id x)
  runAIScript ai GetTime = with ST do
    [pai, cache] <- split ai
    let time' = time !(read pai)
    combine ai [pai, cache]
    pure time'
  runAIScript ai (SetTarget id target_id) = with ST do
    [pai, cache] <- split ai
    update pai $ addInfo target_id
    update pai $ updateController id $ updateData $ setTarget target_id
    combine ai [pai, cache]
  runAIScript ai (GetPosition id) = with ST do
    [pai, cache] <- split ai
    pai' <- read pai
    combine ai [pai, cache]
    pure $ getPosition id pai'
  runAIScript ai (GetDirection id) = queryController ai id direction >>= pure . join
  runAIScript ai (GetStartTime id) = queryController ai id transitioned
  runAIScript ai (GetController id) = getController ai id
  runAIScript ai (Log x) = lift $ GameIO.log x
  runAIScript ai (Pure res) = pure res
  runAIScript ai (x >>= f) = runAIScript ai x >>= (runAIScript ai) . f

  runUnitScripts ai [] = pure ()
  runUnitScripts ai (x :: xs) = runAIScript ai x >>= const (runUnitScripts ai xs)

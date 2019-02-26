module Logic

import Control.ST
import Data.Vect
import Data.Queue
import Data.AVL.Dict

import Events
import Objects

%access public export


-- SScene : Nat -> Nat -> Type
SScene : Type
SScene = Composite [State (List Object),
                    State (List Events.Event)]

startScene : Int -> Int -> ST m Var [add SScene]
startScene x y = do objects <- new []
                    events <- new []
                    scene <- new ()
                    combine scene [objects, events]
                    pure scene

endScene : (scene : Var) -> ST m () [remove scene SScene]
endScene scene = do [objects, events] <- split scene
                    delete objects; delete events; delete scene

registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
registerEvent scene event
  = do [objects, events] <- split scene
       write events (event :: !(read events))
       combine scene [objects, events]

update : (scene : Var) -> ST m (List Events.Event) [scene ::: SScene]
update scene = pure []

-- HERE Composite vs. multiple resources: read the tutorial again

-- clear : (scene : Var) -> ST m Int [scene ::: SScene]
-- present : (scene : Var) -> ST m () [scene ::: SScene]


-- implementation Scene m where
--   SScene = Composite [Queue ]

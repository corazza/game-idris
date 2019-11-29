module Creator.MapCreator

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL2

import Client.UI
import Client.ClientCommands
import Client.Rendering
import Client.Rendering.Camera
import Client.Rendering.RenderMethods
import Client.Rendering.Layers
import Client.Rendering.PositionData
import Client.Rendering.AnimationState
import Client.SDL
import Client.SDL.Points
import Descriptions.ObjectDescription
import Descriptions.ObjectDescription.RenderDescription
import Creator.MapCreator.PMapCreator
import Creator.MapCreator.MapCreatorControl
import Creator.MapCreator.Tools
import Descriptions.MapDescription
import Descriptions.Color
import GameIO
import Objects
import JSONCache
import Settings
import Commands

public export
interface MapCreator (m : Type -> Type) where
  SMapCreator : Maybe Tool -> Type

  startMapCreator : (preload : PreloadResults) ->
                    ST m Var [add $ SMapCreator Nothing]

  endMapCreator : (map_creator : Var) -> ST m () [remove map_creator (SMapCreator ts)]

  queryPMapCreator : (map_creator : Var) ->
                     (q : PMapCreator -> a) ->
                     ST m a [map_creator ::: SMapCreator ts]

  updatePMapCreator : (map_creator : Var) ->
                      (f : PMapCreator -> PMapCreator) ->
                      ST m () [map_creator ::: SMapCreator ts]

  loadMap : (map_creator : Var) ->
            (map_ref : ContentReference) ->
            ST m () [map_creator ::: SMapCreator Nothing]

  getMap : (map_creator : Var) -> ST m (Maybe MapDescription)
                                       [map_creator ::: SMapCreator ts]

  -- maybe remove this too
  getAnimationStateQ : (map_creator : Var) ->
                       ST m (ObjectId -> Maybe AnimationState)
                            [map_creator ::: SMapCreator ts]
  -- the following four functions ought to be removed, not the map editor's concern
  runCommand : (map_creator : Var) ->
               (command : Command) ->
               ST m () [map_creator ::: SMapCreator st]

  applyControls : (map_creator : Var) ->
                  (dt : Double) ->
                  ST m () [map_creator ::: SMapCreator st]

  iterate : (map_creator : Var) ->
            ST m () [map_creator ::: SMapCreator st]

  runClientCommand : (map_creator : Var) ->
                     (clientCommand : ClientCommand) ->
                     ST m () [map_creator ::: SMapCreator st]

  -- EDITING FUNCTIONS
  setTool : (map_creator : Var) ->
            (tool : Tool) ->
            ST m () [map_creator ::: SMapCreator ts :-> SMapCreator (Just tool)]
  unsetTool : (map_creator : Var) ->
              ST m () [map_creator ::: SMapCreator (Just tool) :-> SMapCreator Nothing]

  editAddDynamicAt : (map_creator : Var) ->
                     -- (ref : ContentReference) ->
                     (position : Vector2D) ->
                     ST m () [map_creator ::: SMapCreator (Just $ Add ref)]

  editRemoveCreationFrom : (map_creator : Var) ->
                           (position : Vector2D) ->
                           ST m () [map_creator ::: SMapCreator (Just Remove)]

  editAddRectWall : (map_creator : Var) ->
                    (position : Vector2D) ->
                    (dims : Vector2D) ->
                    ST m () [map_creator ::: SMapCreator (Just AddRectWall)]

  editSetSpawnAt : (map_creator : Var) ->
                   (position : Vector2D) ->
                   ST m () [map_creator ::: SMapCreator (Just SetSpawn)]

  editUpdateMoving : (map_creator : Var) ->
                     ST m () [map_creator ::: SMapCreator (Just Move)]

  editSetPosition : (map_creator : Var) ->
                    (id : ObjectId) ->
                    (position : Vector2D) ->
                    ST m () [map_creator ::: SMapCreator (Just Move)]

  editSetAngle : (map_creator : Var) ->
                 (id : ObjectId) ->
                 (angle : Double) ->
                 ST m () [map_creator ::: SMapCreator (Just Move)]

  private
  addRenderCreator : (map_creator : Var) ->
                     (id : ObjectId) ->
                     (position : PositionData) ->
                     (render_creator : Maybe RenderMethod) ->
                     ST m () [map_creator ::: SMapCreator ts]
  private
  addObject : (map_creator : Var) ->
              (id : ObjectId) ->
              (desc : Maybe ObjectDescription) ->
              (render_creator : Maybe RenderMethod) ->
              (position : PositionData) ->
              ST m () [map_creator ::: SMapCreator ts]

  removeObject : (map_creator : Var) ->
                 (id : ObjectId) ->
                 ST m () [map_creator ::: SMapCreator ts]

  private
  getIdAt : (map_creator : Var) ->
            (position : Vector2D) ->
            ST m (Maybe ObjectId) [map_creator ::: SMapCreator ts]

  private
  newId : (map_creator : Var) -> ST m ObjectId [map_creator ::: SMapCreator ts]
  private
  decideId : (map_creator : Var) -> Creation -> ST m ObjectId [map_creator ::: SMapCreator ts]
  private
  correctDynamicIds' : (map_creator : Var) ->
                       (creations : List Creation) ->
                       ST m (List Creation) [map_creator ::: SMapCreator ts]
  private
  correctDynamicIds : (map_creator : Var) -> ST m () [map_creator ::: SMapCreator ts]
  private
  correctStaticRenders : (map_creator : Var) -> ST m () [map_creator ::: SMapCreator ts]
  private
  addStatic : (map_creator : Var) ->
              List (StaticCreation, ObjectDescription) ->
              ST m () [map_creator ::: SMapCreator ts]
  private
  addDynamic : (map_creator : Var) ->
               List (Creation, ObjectDescription) ->
               ST m () [map_creator ::: SMapCreator ts]
  private
  loadStatic : (map_creator : Var) ->
               MapDescription ->
               ST m (Checked (List (StaticCreation, ObjectDescription)))
                    [map_creator ::: SMapCreator Nothing]
  private
  loadDynamic : (map_creator : Var) ->
                MapDescription ->
                ST m (Checked (List (Creation, ObjectDescription)))
                     [map_creator ::: SMapCreator Nothing]

  private
  zoom : (map_creator : Var) -> (x : Int) -> ST m () [map_creator ::: SMapCreator ts]

export
(GameIO m, SDL m) => MapCreator m where
  SMapCreator Nothing = Composite [State PMapCreator]
  SMapCreator (Just tool) = Composite [State PMapCreator, State $ toolState tool]

  startMapCreator preload = with ST do
    pmap_creator <- new $ initialPMapCreator !ticks preload
    map_creator <- new ()
    combine map_creator [pmap_creator]
    pure map_creator

  endMapCreator map_creator {ts=Nothing} = with ST do
    [pmap_creator] <- split map_creator
    delete pmap_creator
    delete map_creator
  endMapCreator map_creator {ts=Just tool} = with ST do
    [pmap_creator, tool_state] <- split map_creator
    delete tool_state
    delete pmap_creator
    delete map_creator

  queryPMapCreator map_creator q {ts=Nothing} = with ST do
    [pmap_creator] <- split map_creator
    pmap_creator' <- read pmap_creator
    combine map_creator [pmap_creator]
    pure $ q pmap_creator'
  queryPMapCreator map_creator q {ts=Just tool} = with ST do
    [pmap_creator, tool_state] <- split map_creator
    pmap_creator' <- read pmap_creator
    combine map_creator [pmap_creator, tool_state]
    pure $ q pmap_creator'

  updatePMapCreator map_creator f {ts=Nothing} = with ST do
    [pmap_creator] <- split map_creator
    update pmap_creator f
    combine map_creator [pmap_creator]
  updatePMapCreator map_creator f {ts=Just tool} = with ST do
    [pmap_creator, tool_state] <- split map_creator
    update pmap_creator f
    combine map_creator [pmap_creator, tool_state]

  setTool map_creator tool {ts=Nothing} = with ST do
    [pmap_creator] <- split map_creator
    tool_state <- new $ initialToolState tool
    combine map_creator [pmap_creator, tool_state]
  setTool map_creator tool {ts=Just previous} = with ST do
    [pmap_creator, previous_tool_state] <- split map_creator
    delete previous_tool_state
    tool_state <- new $ initialToolState tool
    combine map_creator [pmap_creator, tool_state]

  unsetTool map_creator = with ST do
    [pmap_creator, previous_tool_state] <- split map_creator
    delete previous_tool_state
    combine map_creator [pmap_creator]

  editAddDynamicAt map_creator position {ref} = with ST do
    preload' <- queryPMapCreator map_creator preload {ts=Just (Tools.Add ref)}
    adding_data <- queryPMapCreator map_creator adding {ts=Just (Tools.Add ref)}
    case getObjectDescription ref preload' of
      Left e => lift $ log $
        "(editAddDynamicAt) couldn't get object description, error:\n" ++ e
      Right object_desc => with ST do
        id' <- newId map_creator {ts=Just (Tools.Add ref)}
        let creation = creationForEditor ref position $ Just id'
        updatePMapCreator {ts=Just (Tools.Add ref)} map_creator $
          editAddDynamic creation
        let positionData = noFlip position $ angle adding_data
        addObject map_creator id' (Just object_desc) Nothing positionData
                  {ts=Just (Tools.Add ref)}

  getIdAt map_creator position = with ST do
    posdims <- queryPMapCreator map_creator positions
    layers' <- queryPMapCreator map_creator layers
    pure $ pmapGetIdAt posdims layers' position

  editRemoveCreationFrom map_creator position =
    case !(getIdAt {ts=Just Tools.Remove} map_creator position) of
      Nothing => pure ()
      Just id' => with ST do
        removeObject map_creator id' {ts=Just Tools.Remove}
        updatePMapCreator {ts=Just Tools.Remove} map_creator $ editRemoveDynamic id'
        updatePMapCreator {ts=Just Tools.Remove} map_creator $ editRemoveStatic id'

  editAddRectWall map_creator position dims = with ST do
    id <- newId map_creator {ts=Just AddRectWall}
    adding_data <- queryPMapCreator {ts=Just AddRectWall} map_creator adding
    let creation = creationForRectWall id position dims $ angle adding_data
    updatePMapCreator {ts=Just AddRectWall} map_creator $ editAddStatic creation
    let positionData = noFlip position $ angle adding_data
    addObject {ts=Just AddRectWall} map_creator id Nothing (render creation) positionData

  editSetSpawnAt map_creator position
    = updatePMapCreator {ts=Just SetSpawn} map_creator $ editSetSpawn position

-- HERE
-- separate MapCreator and MapCreatorUI
-- the interface renders the map and the tooling, keeps
-- track of tool state, consumes ui events, commits changes
-- to the MapCreator (core)


--   editUpdateMoving map_creator
--     = case !(queryPMapCreator map_creator $ queryAdding clickedOn) of
--         Nothing => pure ()
--         Just id => case !(queryPMapCreator map_creator $ posdims id) of
--           Nothing => pure ()
--           Just (posdata, dims) => with ST do
--             let position = position posdata
--             let angle = angle posdata
--             position' <- queryPMapCreator map_creator mouseLast
--             angle' <- queryPMapCreator map_creator $ queryAdding AddingData.angle
--             editSetPosition map_creator id position'
--             editSetAngle map_creator id angle'
--
--   editSetPosition map_creator id position = with ST do
--     updatePMapCreator map_creator $ updateMap $ setDynamicPosition id position
--     updatePMapCreator map_creator $ updateMap $ setStaticPosition id position
--     updatePMapCreator map_creator $ setPosition id position
--
--   editSetAngle map_creator id angle = with ST do
--     updatePMapCreator map_creator $ updateMap $ setDynamicAngle id angle
--     updatePMapCreator map_creator $ updateMap $ setStaticAngle id angle
--     updatePMapCreator map_creator $ setAngle id angle
--
--   runCommand map_creator (Start (Movement direction) id)
--     = updatePMapCreator map_creator $ updateControl $ startMoving direction
--   runCommand map_creator (Stop (Movement direction) id)
--     = updatePMapCreator map_creator $ updateControl $ stopMoving direction
--   runCommand map_creator command = pure ()
--
--   applyControls map_creator dt = with ST do
--     velocityVector <- queryPMapCreator map_creator $ queryControl getMove
--     let moveVector = (cameraSpeed * dt) `scale` velocityVector
--     updatePMapCreator map_creator $ updateCamera $ move moveVector
--
--   iterate map_creator = with ST do
--     lastms' <- queryPMapCreator map_creator lastms
--     let newms = !ticks
--     let dt = newms - lastms'
--     updatePMapCreator map_creator $ setLastms newms
--     applyControls map_creator $ (the Double (cast dt)) / 1000.0
--     editUpdateMoving map_creator
--
--   loadMap map_creator map_ref = with ST do
--     preload <- queryPMapCreator map_creator preload
--     case getMapDescription map_ref preload of
--       Left e => lift $ log $
--         "(loadMap map_creator) client couldn't get map description, error:\n" ++ e
--       Right desc => with ST do
--         updatePMapCreator map_creator $ setMap desc
--         correctDynamicIds map_creator
--         correctStaticRenders map_creator
--         Just desc' <- queryPMapCreator map_creator map_desc
--         Right static <- loadStatic map_creator desc' | Left e => with ST do
--           lift $ log $ "map creator couldn't get static, error:\n" ++ e
--         Right dynamic <- loadDynamic map_creator desc' | Left e => with ST do
--           lift $ log $ "map creator couldn't get dynamic, error:\n" ++ e
--         addStatic map_creator static
--         addDynamic map_creator dynamic
--
--   getMap map_creator = queryPMapCreator map_creator map_desc
--
--   -- TODO DUPLICATION (also in server)
--   newId map_creator = with ST do
--     id_num <- queryPMapCreator map_creator idCounter
--     updatePMapCreator map_creator scounter
--     pure $ "map_creator_autoid_" ++ show id_num
--
--   correctStaticRenders map_creator = updatePMapCreator map_creator $
--     updateMap $ generateStaticRenders
--
--   correctDynamicIds' map_creator [] = pure []
--   correctDynamicIds' map_creator (creation::xs) = with ST do
--     id' <- decideId map_creator creation
--     let creation' = setId id' creation
--     rest <- correctDynamicIds' map_creator xs
--     pure $ creation' :: rest
--
--   correctDynamicIds map_creator = with ST do
--     Just map_desc' <- queryPMapCreator map_creator map_desc | pure ()
--     updatePMapCreator map_creator $ updateMap $
--       setDynamic !(correctDynamicIds' map_creator (creations map_desc'))
--
--   decideId map_creator creation = case Creation.id creation of
--     Nothing => newId map_creator
--     Just id' => with ST do
--       positions' <- queryPMapCreator map_creator positions
--       case hasKey id' positions' of
--         False => pure id'
--         True => newId map_creator
--
--   loadStatic map_creator map_description
--     = queryPMapCreator map_creator preload >>=
--         pure . flip getStaticFromMap map_description
--
--   loadDynamic map_creator map_description
--     = queryPMapCreator map_creator preload >>=
--         pure . flip getObjectsFromMap map_description
--
--   addRenderCreator map_creator id positionData Nothing = pure ()
--   addRenderCreator map_creator id positionData (Just method) = with ST do
--     let dims = getDimensions method
--     updatePMapCreator map_creator $ addToPositions id positionData dims
--     updatePMapCreator map_creator $ addToInvisibles id method
--
--   addObject map_creator id Nothing render_creator positionData =
--     addRenderCreator map_creator id positionData render_creator
--   addObject map_creator id (Just desc) render_creator positionData = with ST do
--     addRenderCreator map_creator id positionData render_creator
--     case render desc of
--       Nothing => pure ()
--       Just render_desc => case pickRenderMethod render_desc of
--         Just method => with ST do
--           let dims = getDimensions method
--           updatePMapCreator map_creator $ addToPositions id positionData dims
--           updatePMapCreator map_creator $ updateLayers $ addToLayer id render_desc
--         Nothing => pure ()
--
--   removeObject map_creator id = with ST do
--     updatePMapCreator map_creator $ removeFromPositions id
--     updatePMapCreator map_creator $ updateLayers $ removeFromLayers id
--     updatePMapCreator map_creator $ removeFromInvisibles id
--
--   addStatic map_creator [] = pure ()
--   addStatic map_creator ((creation, desc)::xs) = with ST do
--     addObject map_creator (id creation) (Just desc) (render creation)
--               (fromCreation' creation)
--     addStatic map_creator xs
--
--   addDynamic map_creator [] = pure ()
--   addDynamic map_creator ((creation, desc)::xs) = with ST do
--     id' <- decideId map_creator creation
--     addObject map_creator id' (Just desc) Nothing (fromCreation creation)
--     addDynamic map_creator xs
--
--   getAnimationStateQ map_creator =
--     pure $ dummyAnimationStates' !ticks
--
--   zoom map_creator x =
--     updatePMapCreator map_creator $ updateCamera $ zoomByFactor $ computeZoomFactor x
--
--   runClientCommand map_creator (Stop (Zoom x))
--     = case !(queryPMapCreator map_creator tool) of
--         Just (Add ref) => updatePMapCreator map_creator $ updateAdding $
--           rotateAdding $ angleChange 100 x
--         Just AddRectWall => updatePMapCreator map_creator $ updateAdding $
--           rotateAdding $ angleChange 100 x
--         Just Move => updatePMapCreator map_creator $ updateAdding $
--           rotateAdding $ angleChange 100 x
--         _ => zoom map_creator x
--   runClientCommand map_creator (Mouse (ButtonDown x y)) = with ST do
--     camera' <- queryPMapCreator map_creator camera
--     let at = screenToPosition camera' (x, y)
--     case !(queryPMapCreator map_creator tool) of
--       Just Move => with ST do
--         updatePMapCreator map_creator $ updateAdding $ unsetSelectBegin
--         id <- getIdAt map_creator at
--         updatePMapCreator map_creator $ updateAdding $ setClickedOn' id
--         case id of
--           Nothing => pure ()
--           Just id' => case !(queryPMapCreator map_creator $ posdims id') of
--             Nothing => pure ()
--             Just (posdata, dims) => updatePMapCreator map_creator $
--               updateAdding $ setAngle $ angle posdata
--       _ => updatePMapCreator map_creator $ updateAdding $ setSelectBegin at
--   runClientCommand map_creator (Mouse (ButtonUp x y)) = with ST do
--     adding_data <- queryPMapCreator map_creator adding
--     let selectBegin = selectBegin adding_data
--     updatePMapCreator map_creator $ updateAdding unsetSelectBegin
--     updatePMapCreator map_creator $ updateAdding unsetClickedOn
--     case !(queryPMapCreator map_creator tool) of
--       Just (Add ref) => with ST do
--         camera' <- queryPMapCreator map_creator camera
--         let at = screenToPosition camera' (x, y)
--         editAddDynamicAt map_creator ref at
--       Just Remove => with ST do
--         camera' <- queryPMapCreator map_creator camera
--         let at = screenToPosition camera' (x, y)
--         editRemoveCreationFrom map_creator at
--       Just AddRectWall => with ST do
--         camera <- queryPMapCreator map_creator camera
--         let endpos = screenToPosition camera (x, y)
--         case selectBegin of
--           Nothing => pure ()
--           Just beginpos => with ST do
--             let (position, dims) = getWallPosDims beginpos endpos
--             editAddRectWall map_creator position dims
--       Just SetSpawn => with ST do
--         camera <- queryPMapCreator map_creator camera
--         let at = screenToPosition camera (x, y)
--         editSetSpawnAt map_creator at
--       _ => pure ()
--   runClientCommand map_creator (Mouse (Move x y)) = with ST do
--     camera' <- queryPMapCreator map_creator camera
--     updatePMapCreator map_creator $ setMouseLast $ screenToPosition camera' (x, y)
--   runClientCommand map_creator _ = pure ()
--
-- renderBackground : (SDL m, GameIO m) =>
--                    (map_creator : Var) ->
--                    (sdl : Var) ->
--                    (camera : Camera) ->
--                    ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderBackground map_creator sdl camera = with ST do
--   Just map_desc' <- queryPMapCreator map_creator map_desc | pure ()
--   renderBackground sdl camera $ background map_desc'
--
-- renderSpawn : (SDL m, GameIO m) =>
--               (map_creator : Var) ->
--               (sdl : Var) ->
--               (camera : Camera) ->
--               ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderSpawn map_creator sdl camera = with ST do
--   Just map_desc' <- queryPMapCreator map_creator map_desc | pure ()
--   let spawn_rect = getRect camera (spawn map_desc') (0.5, 0.5)
--   drawWholeCenter sdl "main/images/spawn.png" spawn_rect 0.0 0
--
-- renderInvisibles : SDL m => MapCreator m => GameIO m =>
--                    (map_creator : Var) ->
--                    (sdl : Var) ->
--                    (xs : List (ObjectId, RenderMethod)) ->
--                    ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderInvisibles map_creator sdl [] = pure ()
-- renderInvisibles map_creator sdl ((id, method)::xs) = with ST do
--   positions' <- queryPMapCreator map_creator positions
--   case lookup id positions' of
--     Nothing => renderInvisibles map_creator sdl xs
--     Just (position_data, dims) => with ST do
--       camera' <- queryPMapCreator map_creator camera
--       let position' = position position_data
--       let angle' = angle position_data
--       let flip' = flip position_data
--       preload' <- queryPMapCreator map_creator preload
--       aq <- MapCreator.getAnimationStateQ map_creator
--       let poly = getRotatedRect camera' position' dims angle'
--       filledPolygon sdl poly wallBackground
--       executeMethod aq preload' sdl camera' id position' angle' flip' $ Just method
--       renderInvisibles map_creator sdl xs
--
-- renderLayer : SDL m => MapCreator m => GameIO m =>
--               (map_creator : Var) ->
--               (sdl : Var) ->
--               (layer : List (ObjectId, RenderDescription)) ->
--               ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderLayer map_creator sdl [] = pure ()
-- renderLayer map_creator sdl ((id, render_description)::xs) = with ST do
--   positions' <- queryPMapCreator map_creator positions
--   case lookup id positions' of
--     Nothing =>  with ST do
--       lift $ log $ "renderLayer: no body data for " ++ id
--       renderLayer map_creator sdl xs
--     Just (position_data, dims) => with ST do
--       camera' <- queryPMapCreator map_creator camera
--       let position' = position position_data
--       let angle' = angle position_data
--       let flip' = flip position_data
--       let method' = method render_description
--       let method_creator = method_creator render_description
--       preload' <- queryPMapCreator map_creator preload
--       aq <- MapCreator.getAnimationStateQ map_creator
--       let poly = getRotatedRect camera' position' dims angle'
--       filledPolygon sdl poly objBackground
--       executeMethod aq preload' sdl camera' id position' angle' flip' method'
--       executeMethod aq preload' sdl camera' id position' angle' flip' method_creator
--       renderLayer map_creator sdl xs
--
-- renderLayers : SDL m => MapCreator m => GameIO m =>
--                (map_creator : Var) ->
--                (sdl : Var) ->
--                (layers : LayerList) ->
--                ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderLayers map_creator sdl [] = pure ()
-- renderLayers map_creator sdl (layer::xs)
--   = renderLayer map_creator sdl layer >>=
--       const (renderLayers map_creator sdl xs)
--
-- renderAtMouse : SDL m => MapCreator m => GameIO m =>
--                 (map_creator : Var) ->
--                 (sdl : Var) ->
--                 (ref : ContentReference) ->
--                 ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderAtMouse map_creator sdl ref = with ST do
--   camera <- queryPMapCreator map_creator camera
--   position <- queryPMapCreator map_creator mouseLast
--   let rect = getRect camera position (0.5, 0.5)
--   drawWholeCenter sdl ref rect 0.0 0
--
-- renderTool : SDL m => MapCreator m => GameIO m =>
--              (map_creator : Var) ->
--              (sdl : Var) ->
--              ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderTool map_creator sdl = case !(queryPMapCreator map_creator tool) of
--   Just (Add ref) => with ST do
--     preload' <- queryPMapCreator map_creator preload
--     adding_data <- queryPMapCreator map_creator adding
--     case getObjectDescription ref preload' of
--       Left e => lift $ log $
--         "(renderTool) couldn't find object description, error:\n" ++ e
--       Right object_desc => case render object_desc of
--         Nothing => pure ()
--         Just render_description => with ST do
--           camera' <- queryPMapCreator map_creator camera
--           position' <- queryPMapCreator map_creator mouseLast
--           let angle' = angle adding_data
--           let flip' = 0
--           let method' = method render_description
--           let method_creator = method_creator render_description
--           aq <- MapCreator.getAnimationStateQ map_creator
--           executeMethod aq preload' sdl camera' "adding1" position' angle' flip'
--                         method'
--           executeMethod aq preload' sdl camera' "adding2" position' angle' flip'
--                         method_creator
--   Just AddRectWall => with ST do
--     camera <- queryPMapCreator map_creator camera
--     endpos <- queryPMapCreator map_creator mouseLast
--     adding_data <- queryPMapCreator map_creator adding
--     case selectBegin adding_data of
--       Nothing => with ST do
--         let angle' = radToDeg $ angle adding_data
--         let (x, y) = positionToScreen camera endpos
--         let rect = MkSDLRect (x-10) (y-10) 20 20
--         drawWholeCenter sdl "main/images/metal_ledge.png" rect angle' 0
--       Just beginpos => with ST do
--         let angle = angle adding_data
--         let (position, dims) = getWallPosDims beginpos endpos
--         let poly = getRotatedRect camera position dims angle
--         outlinePolygon sdl poly color_white
--   Just SetSpawn => renderAtMouse map_creator sdl "main/images/spawn.png"
--   Just Move => renderAtMouse map_creator sdl "main/images/ui/move.png"
--   Just Remove => renderAtMouse map_creator sdl "main/images/ui/red-x.png"
--   _ => pure ()
--
-- export
-- renderMapCreator : (SDL m, GameIO m) =>
--                    (map_creator : Var) ->
--                    (sdl : Var) ->
--                    ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
-- renderMapCreator map_creator sdl = with ST do
--   camera' <- queryPMapCreator map_creator camera
--   renderBackground map_creator sdl camera'
--   invisibles' <- queryPMapCreator map_creator (toList . invisibles)
--   renderInvisibles map_creator sdl invisibles'
--   layers <- queryPMapCreator map_creator $ queryLayers layerList
--   renderLayers map_creator sdl layers
--   renderSpawn map_creator sdl camera'
--   renderTool map_creator sdl

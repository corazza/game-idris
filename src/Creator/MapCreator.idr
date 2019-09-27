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
  SMapCreator : Type

  startMapCreator : (preload : PreloadResults) ->
                    ST m Var [add SMapCreator]
  endMapCreator : (map_creator : Var) -> ST m () [remove map_creator SMapCreator]

  queryPMapCreator : (map_creator : Var) ->
                     (q : PMapCreator -> a) ->
                     ST m a [map_creator ::: SMapCreator]
  updatePMapCreator : (map_creator : Var) ->
                      (f : PMapCreator -> PMapCreator) ->
                      ST m () [map_creator ::: SMapCreator]

  loadMap : (map_creator : Var) ->
            (map_ref : ContentReference) ->
            ST m () [map_creator ::: SMapCreator]

  getMap : (map_creator : Var) -> ST m (Maybe MapDescription) [map_creator ::: SMapCreator]

  getAnimationStateQ : (map_creator : Var) ->
                       ST m (ObjectId -> Maybe AnimationState) [map_creator ::: SMapCreator]

  -- TODO think how objects are simultaneously added to layers, positions,
  -- and also edit the map

  runCommand : (map_creator : Var) ->
               (command : Command) ->
               ST m () [map_creator ::: SMapCreator]

  applyControls : (map_creator : Var) ->
                  (dt : Double) ->
                  ST m () [map_creator ::: SMapCreator]

  iterate : (map_creator : Var) ->
            ST m () [map_creator ::: SMapCreator]

  runClientCommand : (map_creator : Var) ->
                     (clientCommand : ClientCommand) ->
                     ST m () [map_creator ::: SMapCreator]

  -- EDITING FUNCTIONS
  setTool : (map_creator : Var) ->
            (tool : Tool) ->
            ST m () [map_creator ::: SMapCreator]
  unsetTool : (map_creator : Var) ->
              ST m () [map_creator ::: SMapCreator]
  editAddDynamicAt : (map_creator : Var) ->
                     (ref : ContentReference) ->
                     (position : Vector2D) ->
                     ST m () [map_creator ::: SMapCreator]
  editRemoveCreationFrom : (map_creator : Var) ->
                           (position : Vector2D) ->
                           ST m () [map_creator ::: SMapCreator]

  private
  addObject : (map_creator : Var) ->
              (id : ObjectId) ->
              (desc : ObjectDescription) ->
              (position : PositionData) ->
              ST m () [map_creator ::: SMapCreator]

  removeObject : (map_creator : Var) ->
                 (id : ObjectId) ->
                 ST m () [map_creator ::: SMapCreator]

  private
  newId : (map_creator : Var) -> ST m ObjectId [map_creator ::: SMapCreator]
  private
  decideId : (map_creator : Var) -> Creation -> ST m ObjectId [map_creator ::: SMapCreator]
  private
  addStatic : (map_creator : Var) ->
              List (StaticCreation, ObjectDescription) ->
              ST m () [map_creator ::: SMapCreator]
  private
  addDynamic : (map_creator : Var) ->
               List (Creation, ObjectDescription) ->
               ST m () [map_creator ::: SMapCreator]
  private
  loadStatic : (map_creator : Var) ->
               MapDescription ->
               ST m (Checked (List (StaticCreation, ObjectDescription)))
                    [map_creator ::: SMapCreator]
  private
  loadDynamic : (map_creator : Var) ->
                MapDescription ->
                ST m (Checked (List (Creation, ObjectDescription)))
                     [map_creator ::: SMapCreator]

  private
  zoom : (map_creator : Var) -> (x : Int) -> ST m () [map_creator ::: SMapCreator]

export
(GameIO m, SDL m) => MapCreator m where
  SMapCreator = Composite [State PMapCreator]

  startMapCreator preload = with ST do
    pmap_creator <- new $ initialPMapCreator !ticks preload
    map_creator <- new ()
    combine map_creator [pmap_creator]
    pure map_creator

  endMapCreator map_creator = with ST do
    [pmap_creator] <- split map_creator
    delete pmap_creator
    delete map_creator

  queryPMapCreator map_creator q = with ST do
    [pmap_creator] <- split map_creator
    pmap_creator' <- read pmap_creator
    combine map_creator [pmap_creator]
    pure $ q pmap_creator'

  updatePMapCreator map_creator f = with ST do
    [pmap_creator] <- split map_creator
    update pmap_creator f
    combine map_creator [pmap_creator]

  setTool map_creator tool = updatePMapCreator map_creator $ pmapSetTool tool
  unsetTool map_creator = updatePMapCreator map_creator pmapUnsetTool

  editAddDynamicAt map_creator ref position = with ST do
    preload' <- queryPMapCreator map_creator preload
    adding_data <- queryPMapCreator map_creator adding
    case getObjectDescription ref preload' of
      Left e => lift $ log $
        "(editAddDynamicAt) couldn't get object description, error:\n" ++ e
      Right object_desc => with ST do
        id' <- newId map_creator
        let creation = creationForEditor ref position $ Just id'
        updatePMapCreator map_creator $ editAddDynamic creation
        let positionData = noFlip position $ angle adding_data
        addObject map_creator id' object_desc positionData

  editRemoveCreationFrom map_creator position = with ST do
    posdims <- queryPMapCreator map_creator positions
    layers' <- queryPMapCreator map_creator layers
    case getIdAt posdims layers' position of
      Nothing => pure ()
      Just id => removeObject map_creator id

  runCommand map_creator (Start (Movement direction) id)
    = updatePMapCreator map_creator $ updateControl $ startMoving direction
  runCommand map_creator (Stop (Movement direction) id)
    = updatePMapCreator map_creator $ updateControl $ stopMoving direction
  runCommand map_creator command = pure ()

  applyControls map_creator dt = with ST do
    velocityVector <- queryPMapCreator map_creator $ queryControl getMove
    let moveVector = (cameraSpeed * dt) `scale` velocityVector
    updatePMapCreator map_creator $ updateCamera $ move moveVector

  iterate map_creator = with ST do
    lastms' <- queryPMapCreator map_creator lastms
    let newms = !ticks
    let dt = newms - lastms'
    updatePMapCreator map_creator $ setLastms newms
    applyControls map_creator $ (the Double (cast dt)) / 1000.0

  loadMap map_creator map_ref = with ST do
    preload <- queryPMapCreator map_creator preload
    case getMapDescription map_ref preload of
      Left e => lift $ log $
        "(loadMap map_creator) client couldn't get map description, error:\n" ++ e
      Right desc => with ST do
        updatePMapCreator map_creator $ setMap desc
        Right static <- loadStatic map_creator desc | Left e => with ST do
          lift $ log $ "map creator couldn't get static, error:\n" ++ e
        Right dynamic <- loadDynamic map_creator desc | Left e => with ST do
          lift $ log $ "map creator couldn't get dynamic, error:\n" ++ e
        addStatic map_creator static
        addDynamic map_creator dynamic

  getMap map_creator = queryPMapCreator map_creator map_desc

  -- TODO DUPLICATION (also in server)
  newId map_creator = with ST do
    id_num <- queryPMapCreator map_creator idCounter
    updatePMapCreator map_creator scounter
    pure $ "map_creator_autoid_" ++ show id_num

  decideId map_creator creation = case Creation.id creation of
    Nothing => newId map_creator
    Just id' => with ST do
      positions' <- queryPMapCreator map_creator positions
      case hasKey id' positions' of
        False => pure id'
        True => newId map_creator

  loadStatic map_creator map_description
    = queryPMapCreator map_creator preload >>=
        pure . flip getStaticFromMap map_description

  loadDynamic map_creator map_description
    = queryPMapCreator map_creator preload >>=
        pure . flip getObjectsFromMap map_description

  addObject map_creator id desc positionData = case render desc of
    Nothing => pure ()
    Just render_desc => with ST do
      let dims = getDimensions $ method render_desc
      updatePMapCreator map_creator $ updateLayers $ addToLayer id render_desc
      updatePMapCreator map_creator $ addToPositions id positionData dims

  removeObject map_creator id = with ST do
    updatePMapCreator map_creator $ updateLayers $ removeFromLayers id


  addStatic map_creator [] = pure ()
  addStatic map_creator ((creation, desc)::xs)
    = addObject map_creator (id creation) desc (fromCreation' creation) >>=
        const (addStatic map_creator xs)

  addDynamic map_creator [] = pure ()
  addDynamic map_creator ((creation, desc)::xs) = with ST do
    id' <- decideId map_creator creation
    addObject map_creator id' desc (fromCreation creation) >>=
      const (addDynamic map_creator xs)

  getAnimationStateQ map_creator =
    pure $ dummyAnimationStates' !ticks

  zoom map_creator x =
    updatePMapCreator map_creator $ updateCamera $ zoomFactor $ computeZoomFactor x

  runClientCommand map_creator (Stop (Zoom x))
    = case !(queryPMapCreator map_creator tool) of
        Just (Add ref) =>
          updatePMapCreator map_creator $ pmapRotateAdding $ angleChange x
        _ => zoom map_creator x
  runClientCommand map_creator (Mouse (ButtonDown x y)) = pure ()
  runClientCommand map_creator (Mouse (ButtonUp x y))
    = case !(queryPMapCreator map_creator tool) of
        Just (Add ref) => with ST do
          camera' <- queryPMapCreator map_creator camera
          let at = screenToPosition camera' (x, y)
          editAddDynamicAt map_creator ref at
        Just Remove => with ST do
          camera' <- queryPMapCreator map_creator camera
          let at = screenToPosition camera' (x, y)
          editRemoveCreationFrom map_creator at
        _ => pure ()
  runClientCommand map_creator (Mouse (Move x y)) = with ST do
    camera' <- queryPMapCreator map_creator camera
    updatePMapCreator map_creator $ setMouseLast $ screenToPosition camera' (x, y)


renderBackground : (SDL m, GameIO m) =>
                     (map_creator : Var) ->
                     (sdl : Var) ->
                     (camera : Camera) ->
                     ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
renderBackground map_creator sdl camera = with ST do
  Just map_desc' <- queryPMapCreator map_creator map_desc | pure ()
  renderBackground sdl camera $ background map_desc'

renderLayer : SDL m => MapCreator m => GameIO m =>
              (map_creator : Var) ->
              (sdl : Var) ->
              (layer : List (ObjectId, RenderDescription)) ->
              ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
renderLayer map_creator sdl [] = pure ()
renderLayer map_creator sdl ((id, render_description)::xs) = with ST do
  positions' <- queryPMapCreator map_creator positions
  case lookup id positions' of
    Nothing =>  with ST do
      lift $ log $ "renderLayer: no body data for " ++ id
      renderLayer map_creator sdl xs
    Just (position_data, dims) => with ST do
      camera' <- queryPMapCreator map_creator camera
      let position' = position position_data
      let angle' = radToDeg $ angle position_data
      let flip' = flip position_data
      let method' = method render_description
      preload' <- queryPMapCreator map_creator preload
      aq <- MapCreator.getAnimationStateQ map_creator
      executeMethod aq preload' sdl camera' id position' angle' flip' method'
      renderLayer map_creator sdl xs

renderLayers : SDL m => MapCreator m => GameIO m =>
               (map_creator : Var) ->
               (sdl : Var) ->
               (layers : LayerList) ->
               ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
renderLayers map_creator sdl [] = pure ()
renderLayers map_creator sdl (layer::xs)
  = renderLayer map_creator sdl layer >>=
      const (renderLayers map_creator sdl xs)

renderTool : SDL m => MapCreator m => GameIO m =>
             (map_creator : Var) ->
             (sdl : Var) ->
             ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
renderTool map_creator sdl = case !(queryPMapCreator map_creator tool) of
  Just (Add ref) => with ST do
    preload' <- queryPMapCreator map_creator preload
    adding_data <- queryPMapCreator map_creator adding
    case getObjectDescription ref preload' of
      Left e => lift $ log $
        "(renderTool) couldn't find object description, error:\n" ++ e
      Right object_desc => case render object_desc of
        Nothing => pure ()
        Just render_description => with ST do
          camera' <- queryPMapCreator map_creator camera
          position' <- queryPMapCreator map_creator mouseLast
          let angle' = radToDeg $ angle adding_data
          let flip' = 0
          let method' = method render_description
          aq <- MapCreator.getAnimationStateQ map_creator
          executeMethod aq preload' sdl camera' "adding" position' angle' flip' method'
  Just Remove => with ST do
    camera' <- queryPMapCreator map_creator camera
    position <- queryPMapCreator map_creator mouseLast
    let (x, y) = positionToScreen camera' position
    let rect = MkSDLRect (x-25) (y-25) 50 50
    drawWholeCenter sdl "main/images/ui/red-x.png" rect 0.0 0
  _ => pure ()

export
renderMapCreator : (SDL m, GameIO m) =>
                   (map_creator : Var) ->
                   (sdl : Var) ->
                   ST m () [map_creator ::: SMapCreator {m}, sdl ::: SSDL {m}]
renderMapCreator map_creator sdl = with ST do
  camera' <- queryPMapCreator map_creator camera
  renderBackground map_creator sdl camera'
  layers <- queryPMapCreator map_creator $ queryLayers layerList
  renderLayers map_creator sdl layers
  renderTool map_creator sdl

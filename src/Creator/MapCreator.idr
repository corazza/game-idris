module Creator.MapCreator

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL2

import Client.UI
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

  getAnimationStateQ : (map_creator : Var) ->
                       ST m (ObjectId -> Maybe AnimationState) [map_creator ::: SMapCreator]

  -- TODO think how objects are simultaneously added to layers, positions,
  -- and also edit the map
  addObject : (map_creator : Var) ->
              (id : ObjectId) ->
              (desc : ObjectDescription) ->
              (position : PositionData) ->
              ST m () [map_creator ::: SMapCreator]


  runCommand : (map_creator : Var) ->
               (command : Command) ->
               ST m () [map_creator ::: SMapCreator]

  applyControls : (map_creator : Var) ->
                  (dt : Double) ->
                  ST m () [map_creator ::: SMapCreator]

  iterate : (map_creator : Var) ->
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
      updatePMapCreator map_creator $ updateLayers $ addToLayer id render_desc
      updatePMapCreator map_creator $ addToPositions id positionData

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
    Just position_data => with ST do
      camera' <- queryPMapCreator map_creator camera
      let position' = position position_data
      let angle' = angle position_data
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

module Creator

import Graphics.SDL2

import Client.UI
import Client.Rendering
import Client.Rendering.Camera
import Client.Input
import Client.SDL
import Client.ClientCommands
import Creator.PCreator
import Creator.MapCreator
import Creator.MapCreator.PMapCreator
import Descriptions.MapDescription
import GameIO
import Objects
import JSONCache
import Settings
import Commands

public export
interface Creator (m : Type -> Type) where
  SCreator : Type

  startCreator : (settings : ClientSettings) ->
                 (preload : PreloadResults) ->
                 ST m Var [add SCreator]
  endCreator : (creator : Var) -> ST m () [remove creator SCreator]

  queryPCreator : (creator : Var) ->
                  (q : PCreator -> a) ->
                  ST m a [creator ::: SCreator]
  updatePCreator : (creator : Var) ->
                   (f : PCreator -> PCreator) ->
                   ST m () [creator ::: SCreator]

  queryPMapCreator : (creator : Var) ->
                     (q : PMapCreator -> a) ->
                     ST m a [creator ::: SCreator]
  updatePMapCreator : (creator : Var) ->
                      (f : PMapCreator -> PMapCreator) ->
                      ST m () [creator ::: SCreator]

  loadMap : (creator : Var) ->
            (map_ref : ContentReference) ->
            ST m () [creator ::: SCreator]

  iterate : (creator : Var) ->
            ST m Bool [creator ::: SCreator]

  private
  render : (creator : Var) ->
           ST m () [creator ::: SCreator]

  private
  runClientCommand : (creator : Var) ->
                     (clientCommand : ClientCommand) ->
                     ST m () [creator ::: SCreator]
  private
  runClientCommands : (creator : Var) ->
                      (clientCommands : List ClientCommand) ->
                      ST m () [creator ::: SCreator]

  private
  runCommand : (creator : Var) ->
               (command : Command) ->
               ST m () [creator ::: SCreator]
  private
  runCommands : (creator : Var) ->
                (commands : List Command) ->
                ST m () [creator ::: SCreator]

export
(GameIO m, Rendering m, SDL m) => Creator m where
  SCreator = Composite [
    State PCreator,
    SSDL {m},
    SMapCreator {m}
  ]

  startCreator settings preload = with ST do
    pcreator <- new $ MkPCreator preload
    sdl <- startSDL (resolutionX settings) (resolutionY settings) preload
    map_creator <- startMapCreator preload
    creator <- new ()
    combine creator [pcreator, sdl, map_creator]
    pure creator

  endCreator creator = with ST do
    [pcreator, sdl, map_creator] <- split creator
    endSDL sdl
    endMapCreator map_creator
    delete pcreator
    delete creator

  queryPCreator creator q = with ST do
    [pcreator, sdl, map_creator] <- split creator
    pcreator' <- read pcreator
    combine creator [pcreator, sdl, map_creator]
    pure $ q pcreator'

  updatePCreator creator f = with ST do
    [pcreator, sdl, map_creator] <- split creator
    update pcreator f
    combine creator [pcreator, sdl, map_creator]

  queryPMapCreator creator q = with ST do
    [pcreator, sdl, map_creator] <- split creator
    result <- MapCreator.queryPMapCreator map_creator q
    combine creator [pcreator, sdl, map_creator]
    pure result

  updatePMapCreator creator f = with ST do
    [pcreator, sdl, map_creator] <- split creator
    MapCreator.updatePMapCreator map_creator f
    combine creator [pcreator, sdl, map_creator]

  loadMap creator map_ref = with ST do
    [pcreator, sdl, map_creator] <- split creator
    MapCreator.loadMap map_creator map_ref
    combine creator [pcreator, sdl, map_creator]

  render creator = with ST do
    [pcreator, sdl, map_creator] <- split creator
    clear sdl
    renderMapCreator map_creator sdl
    present sdl
    combine creator [pcreator, sdl, map_creator]

  runClientCommand creator cmd = pure ()

  runClientCommands creator [] = pure ()
  runClientCommands creator (cmd::xs) = with ST do
    runClientCommand creator cmd
    runClientCommands creator xs

  runCommand creator command = with ST do
    [pcreator, sdl, map_creator] <- split creator
    MapCreator.runCommand map_creator command
    combine creator [pcreator, sdl, map_creator]

  runCommands creator [] = pure ()
  runCommands creator (cmd::xs)
    = runCommand creator cmd >>= const (runCommands creator xs)

  iterate creator = with ST do
    render creator
    sdl_events <- poll
    camera <- queryPMapCreator creator camera
    case processEvents "map creator" camera sdl_events of
      Right (clientCommands, commands) => with ST do
        runClientCommands creator clientCommands
        runCommands creator commands
        [pcreator, sdl, map_creator] <- split creator
        MapCreator.iterate map_creator
        combine creator [pcreator, sdl, map_creator]
        pure True
        -- ?hmmm
        -- clientCommands' <- feedUI client clientCommands
        -- fromClient <- runClientCommands client clientCommands' []
        -- clicks <- getClicks client
        -- clickCommands <- processClicks client clicks []
        -- movement <- queryPClient client lastFacing {s=Connected}
        -- let filtered = filterMovement movement commands
        -- let newCommands = clickCommands ++ fromClient ++ filtered
        -- runCommands client newCommands
        -- setNewFacing client filtered
        -- pure $ Right newCommands
        -- pure True
      _ => pure False

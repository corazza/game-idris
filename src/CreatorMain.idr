module Main

import Control.ST
import Control.ST.ImplicitCall

import Creator
import GameIO
import Commands
import JSONCache
import Descriptions.MapDescription
import Settings
import Timeline
import Exception
import Objects
import Client
import Client.Rendering
import Client.SDL
import Client.PClient
import Client.ClientCommands

loop : (GameIO m, Creator m) =>
       (creator : Var) ->
       ST m () [creator ::: SCreator {m}]
loop creator = case !(iterate creator) of
  False => pure ()
  True => loop creator

start : (GameIO m, Creator m) => String -> ST m () []
start map_ref = with ST do
  Right preload_info <- lift $ checkedJSONLoad {r=Preload} "res/main/preload.json"
        | Left e => with ST do
              lift $ log "couldn't load preload info, error:"
              lift $ log e
  Right preload <- preloadResults preload_info
        | Left e => with ST do
              lift $ log "couldn't load preload, error:"
              lift $ log e

  creator <- startCreator defaultClientSettings preload
  loadMap creator map_ref
  loop creator
  map_result <- getMap creator
  endCreator creator
  case map_result of
    Nothing => pure ()
    Just map_desc => lift $
      GameIO.write (pretty map_desc) "res_work/creator/output.json"

main : IO ()
main = with IO do
  disableBuffering
  args <- getArgs
  case args of
    [prog, map_ref] => run $ start map_ref
    _ => log $ "provide name of map to edit, e.g. \"main/maps/castle.json\""

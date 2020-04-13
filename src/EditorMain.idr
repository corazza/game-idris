module Main

import Control.ST
import Control.ST.ImplicitCall

import Editor
import GameIO
import Commands
import JSONCache
import Descriptions.MapDescription
import Settings
import Timeline
import Exception
import Objects
import Client.Rendering
import Client.SDL
import Client.PClient
import Client.ClientCommands

loop : (GameIO m, Editor m) =>
       (editor : Var) ->
       ST m () [editor ::: SEditor {m}]
loop editor = case !(iterate editor) of
  False => pure ()
  True => loop editor

start : (GameIO m, Editor m) => String -> ST m () []
start map_ref = with ST do
  Right preload_info <- lift $ checkedJSONLoad {r=Preload} "res/main/preload.json"
        | Left e => with ST do
              lift $ log "couldn't load preload info, error:"
              lift $ log e
  Right preload <- preloadResults preload_info
        | Left e => with ST do
              lift $ log "couldn't load preload, error:"
              lift $ log e

  editor <- startEditor defaultClientSettings preload
  loadMap editor map_ref
  toggleRoot editor "main/ui/editor/tools.json" 50 50
  loop editor
  map_result <- getMap editor
  endEditor editor
  case map_result of
    Nothing => pure ()
    Just map_desc => lift $
      GameIO.write (pretty map_desc) "res_work/editor/output.json"

main : IO ()
main = with IO do
  disableBuffering
  args <- getArgs
  case args of
    [_, map_ref] => run $ start map_ref
    _ => log $ "provide name of map to edit, e.g. \"main/maps/castle.json\""

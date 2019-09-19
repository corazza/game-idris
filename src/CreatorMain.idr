module Main

import Control.ST
import Control.ST.ImplicitCall

import Creator
import GameIO
import Commands
import JSONCache
import Descriptions
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

start : (GameIO m, Creator m) => ST m () []
start = with ST do
  Right preload_info <- lift $ checkedJSONLoad {r=Preload} "res/main/preload.json"
        | Left e => with ST do
              lift $ log "couldn't load preload info, error:"
              lift $ log e
  Right preload <- preloadResults preload_info
        | Left e => with ST do
              lift $ log "couldn't load preload, error:"
              lift $ log e

  creator <- startCreator defaultClientSettings preload
  loadMap creator "main/maps/castle.json"
  loop creator
  endCreator creator

main : IO ()
main = with IO do
  disableBuffering
  run start

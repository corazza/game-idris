module Main

import Control.ST
import Control.ST.ImplicitCall

import GameIO
import Dynamics
import Dynamics.PDynamics
import Commands
import JSONCache
import Descriptions
import Settings
import Exception
import Server
import Server.PServer
import Client
import Client.Rendering
import Client.SDL

start : (GameIO m, Dynamics m, Client m, Server m) => ST m () []
start = with ST do
  Right preload_info <- lift $ checkedJSONLoad {r=Preload} "res/main/preload.json"
        | Left e => with ST do
              lift $ log "couldn't load preload, error:"
              lift $ log e

  preload <- preloadResults preload_info
  dynamics <- startDynamics defaultDynamicsSettings
  Right server <- startServer defaultServerSettings "main/maps/castle.json" preload
              | Left e => with ST do
                  lift $ log $ "couldn't start server, error: "
                  lift $ log e
                  endDynamics dynamics
  (serverCommands, dynamicsCommands) <- Server.iterate server 0
  lift $ log $ show serverCommands
  lift $ log $ show dynamicsCommands
  endServer server
  endDynamics dynamics

main : IO ()
main = with IO do
  disableBuffering
  run start

# Client-Server model

For now, the server just sends `Create id ref params` commands to the client which simply adds render methods to its object store. Later, the server will have shortcut commands such as `LoadMap ref [ids]` (? ids for static objects).

Maps only store static bodies--they can be loaded by the server and the client at separate moments and no state syncing is required. Dynamic bodies however must always be sent to the client on connection.

# Components

The code is divided into major components with separate concerns, implemented as Idris interfaces via `Control.ST`. Any task (such as **Rendering**), qualifies as a component if it requires internal state of its own and is independent in relation to other components. Components can wrap other stateful interfaces under themselves, exposing only a general interface. For example, the **Rendering** component wraps the **SDLDraw** interface (by initializing, calling, and cleaning it) and no other part of the code uses `SDLDraw`.

Components attach internal state objects to global identifiers, and can receive and emit updates about these objects. For example, the **Dynamics** component receives updates about relevant control events (along the lines of *id start moving left*), and emits positional updates tied to the relevant ids. There are also operations that create and delete these objects.

## Dynamics

Handles physics and movement/control.

**State:** box2d physics world and body ids, positions etc., control states

**Objects:** box2d id, control state, collision state

## Rendering

Handles animations, draw layers, various types of rendering (tiling, single image, etc.)

**Objects:** animation states, render descriptions

## AI



## Rules

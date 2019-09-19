module Client.Rendering.RenderMethods

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL2

import Client.SDL
import Client.Rendering.Camera
import Client.Rendering.AnimationState
import Descriptions.MapDescription
import Descriptions.ItemDescription
import Descriptions.Color
import Descriptions.AnimationDescription
import Descriptions.ObjectDescription.RenderDescription
import GameIO
import Objects
import JSONCache

export
correctFacing : Bool -> Int -> Int
correctFacing True x = x
correctFacing False 2 = 0
correctFacing False 0 = 2
correctFacing _ x = x

export
flipOffset : Int -> Vector2D -> Vector2D
flipOffset 2 (x, y) = (-x, y)
flipOffset _ (x, y) = (x, y)

export
missingStateError : (state : String) -> (for : ObjectId) -> String
missingStateError state for
  = "couldn't get animation parameters for state \"" ++ state ++ "\""  ++ " for " ++ for

export
missingDescription : (ref : ContentReference) ->
                     (for : ObjectId) ->
                     (error : String) ->
                     String
missingDescription ref for error
  =  "missing animation description " ++ ref ++ " for " ++ for ++ " error:\n" ++ error

export
renderBackground : SDL m =>
                   (sdl : Var) ->
                   (camera : Camera) ->
                   (background : Background) ->
                   ST m () [sdl ::: SSDL {m}]
renderBackground sdl camera (MkBackground image dim) = with ST do
  let (w, h) = dimToScreen camera (2 `scale` dim)
  let (w', h') = dimToScreen camera dim
  let (x, y) = positionToScreen camera (0, 0)
  drawWholeCenter sdl image (MkSDLRect (x - w') (y - h') w h) 0.0 0

export
tile : SDL m =>
       (sdl : Var) ->
       (camera : Camera) ->
       (texture : ContentReference) ->
       (position : (Int, Int)) ->
       (tileDims : (Int, Int)) ->
       (howMany : (Nat, Nat)) ->
       ST m () [sdl ::: SSDL {m}]
tile sdl camera texture (x, y) (w, h) (nx, Z) = pure ()
tile {m} sdl camera texture (x, y) (w, h) (nx, S ny) = (with ST do
  tileRow (x, y) nx
  tile sdl camera texture (x, y + h) (w, h) (nx, ny)) where
    tileRow : (position : (Int, Int)) -> (k : Nat) -> ST m () [sdl ::: SSDL {m}]
    tileRow _ Z = pure ()
    tileRow (x', y') (S k) = with ST do
      let rect = MkSDLRect x' y' w h
      drawWholeCenter sdl texture rect 0.0 0
      tileRow (x' + w, y') k

export
executeMethod : SDL m => GameIO m =>
               (aq : ObjectId -> Maybe AnimationState) ->
               (preload : PreloadResults) ->
               (sdl : Var) ->
               (camera : Camera) ->
               (id : ObjectId) ->
               (position : Vector2D) ->
               (angle : Double) ->
               (flip : Int) ->
               (rendering_description : RenderMethod) ->
               ST m () [sdl ::: SSDL {m}]
executeMethod aq preload sdl camera id position angle flip Invisible = pure ()
executeMethod aq preload sdl camera id position angle flip (Tiled ref tileDims@(w, h) howMany@(nx, ny))
  = let tileDimsFull = dimToScreen camera (2 `scale` tileDims)
        topleft = position - (cast nx * w, - cast ny * h)
        initial = positionToScreen camera topleft
        in tile sdl camera ref initial tileDimsFull (nx, ny)
executeMethod aq preload sdl camera id position angle flip (ColoredRect color dims)
  = let rect = getRect camera position dims
        in filledRect sdl rect color
executeMethod aq preload sdl camera id position angle flip (ColoredCircle color radius) = pure ()
executeMethod aq preload sdl camera id position angle flip (Single ref dims facingRight)
  = let rect = getRect camera position dims
        in drawWholeCenter sdl ref rect angle (correctFacing facingRight flip)
executeMethod aq preload sdl camera id position angle flip (Animated state_dict) =
  case aq id of
    Nothing => pure ()
    Just (MkAnimationState name started attackShowing) =>
      case lookup name state_dict of
        Nothing => pure ()
        Just aparams => with ST do
          let animation_ref = ref aparams
          case getAnimationDescription animation_ref preload of
            Left e => lift $ log $ missingDescription animation_ref id e
            Right animation_description =>
              let src = getSrc !ticks (fps aparams) animation_description
                  dimensions' = dimensions aparams
                  dst = getRect camera position dimensions'
                  sheet = sheet animation_description
                  flip = correctFacing (facingRight animation_description) flip
                  in drawCenter sdl sheet src dst angle flip

export
renderEquipment : SDL m => GameIO m =>
                  (aq : ObjectId -> Maybe AnimationState) ->
                  (preload : PreloadResults) ->
                  (sdl : Var) ->
                  (camera : Camera) ->
                  (eq_id : ObjectId) ->
                  (position : Vector2D) ->
                  (offset : Vector2D) ->
                  (angle : Double) ->
                  (flip : Int) ->
                  (carry_dims : Vector2D) ->
                  (ref : ContentReference) ->
                  ST m () [sdl ::: SSDL {m}]
renderEquipment aq preload sdl camera eq_id position offset angle flip carry_dims ref
  = case getItemDescription ref preload of
      Left e => lift $ log $
        "couldn't get item description for " ++ ref ++ " (executeMethod)"
      Right item_desc => case attackRender item_desc of
        Nothing => pure ()
        Just method =>
          let offset' = flipOffset flip (offset - item_offset item_desc)
              position' = position + offset'
              in executeMethod
                  aq preload sdl camera eq_id position' angle flip method

module ExampleTest where

import Prelude
import Control.Monad.Eff.Alert (Alert, alert)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, runST)
import Control.Monad.Eff.Console (CONSOLE, log)
import System.Clock (CLOCK, milliseconds)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Array (delete, elemIndex, (:), null)
import Data.Array as Array
import Math (pi)
import Data.Int (toNumber)
import Partial.Unsafe (unsafePartial)

import Graphics.WebGLAll (EffWebGL, WebGLTex, Buffer, WebGLProg, WebGLContext, Float, Uniform, Vec3, Bool, Sampler2D, Mat4, Vec2,
        Attribute, BlendingFactor(ONE, SRC_ALPHA), BufferTarget(ELEMENT_ARRAY_BUFFER), Capacity(DEPTH_TEST, BLEND), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT),
        Mode(TRIANGLES), Shaders(Shaders), TexFilterSpec(MIPMAP), setUniformFloats, setUniformBoolean, enable, disable, blendFunc, drawElements,
        bindBuf, withTexture2D, bindBufAndSetVertexAttr, clear, viewport, getCanvasHeight, getCanvasWidth, requestAnimationFrame, texture2DFor,
        clearColor, makeBuffer, makeBufferFloat, withShaders, runWebGL)
import Data.Matrix (toArray) as M
import Data.Matrix4 (identity, translate, rotate, makePerspective) as M
import Data.Matrix3 (normalFromMat4)
import Data.Vector (toArray, normalize, scale) as V
import Data.Vector3 (vec3, vec3') as V
import Data.ArrayBuffer.Types (Uint16, Float32) as T
import Data.TypedArray (asUint16Array) as T
import KeyEvent (Event, eventGetKeyCode, getElementByIdFloat, getElementByIdBool, onKeyUp, onKeyDown)

import Model.MyBindings
import Model.Renderable
import Model.Cube as MC

type BindingsType = {webGLProgram :: WebGLProg | MyBindings}
    
data ContainedObject
    = CubeObject MC.Model
    
data ObjectContainer = ObjectContainer
    { xRot :: Number
    , xSpeed :: Number
    , yRot :: Number
    , ySpeed :: Number
    , x :: Number
    , y :: Number
    , z :: Number
    , obj :: ContainedObject
    }

type State =
    { context :: WebGLContext
    , bindings :: BindingsType

    , cubes :: Array ObjectContainer

    , lastTime :: Maybe Number
    , currentlyPressedKeys :: Array Int
    }

renderable (CubeObject c) = c
    
renderTransformed s bindings = do
  let mvMatrix = M.rotate (degToRad s.yRot) (V.vec3' [0.0, 1.0, 0.0])
                    $ M.rotate (degToRad s.xRot) (V.vec3' [1.0, 0.0, 0.0])
                      $ M.translate  (V.vec3 s.x s.y s.z)
                        $ M.identity
                          
  setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

  let nMatrix = unsafePartial $ fromJust $ normalFromMat4 mvMatrix
  setUniformFloats bindings.uNMatrix (M.toArray nMatrix)

  render (renderable s.obj) bindings
    
instance renderContainedObject :: Renderable ObjectContainer where
    render (ObjectContainer o) bindings = do
        renderTransformed o bindings
    
main :: Eff (console :: CONSOLE, alert :: Alert, clock :: CLOCK) Unit
main = do
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders
            shaders
            (\s -> alert s)
            \ bindings -> do
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "glass.gif" MIPMAP \texture -> do
            cube <- MC.init texture
            let state =
                    { context : context
                    , bindings : bindings

                    , cubes :
                           [ ObjectContainer
                               { xRot : 0.0
                               , xSpeed : 1.0
                               , yRot : 0.0
                               , ySpeed : 1.0
                               , x : 0.0
                               , y : 0.0
                               , z : (-5.0)
                               , obj : CubeObject cube
                               }
                           , ObjectContainer
                               { xRot : 0.0
                               , xSpeed : 1.5
                               , yRot : 0.0
                               , ySpeed : 1.0
                               , x : 3.0
                               , y : 0.0
                               , z : (-5.0)
                               , obj : CubeObject cube
                               }
                           ]
                    , lastTime : Nothing

                    , currentlyPressedKeys : []
                    } :: State
            runST do
              stRef <- newSTRef state
              onKeyDown (handleKeyD stRef)
              onKeyUp (handleKeyU stRef)
              tick stRef

tick :: forall h eff. STRef h State ->  EffWebGL (st :: ST h, console :: CONSOLE, clock :: CLOCK |eff) Unit
tick stRef = do
  drawScene stRef
  handleKeys stRef
  animate stRef
  requestAnimationFrame (tick stRef)

animate ::  forall h eff . STRef h State -> EffWebGL (st :: ST h, clock :: CLOCK |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  timeNow <- milliseconds

  let cubes =
          map
              (\(ObjectContainer obj) ->
                   case s.lastTime of
                     Nothing -> ObjectContainer obj
                     Just lastt ->
                         let elapsed = timeNow - lastt in
                         ObjectContainer
                           (obj
                            { xRot = obj.xRot + obj.xSpeed * elapsed / 1000.0
                            , yRot = obj.yRot + obj.ySpeed * elapsed / 1000.0
                            }
                           )
              )
              s.cubes

  _ <- writeSTRef stRef (s { lastTime = Just timeNow, cubes = cubes })

  pure unit

renderAll :: forall eff. Int -> BindingsType -> Array ObjectContainer -> EffWebGL (eff) Unit
renderAll n bindings array =
    case Array.index array n of
      Just cube -> do
        _ <- render cube bindings
        renderAll (n+1) bindings array
      Nothing -> do
        pure unit
       
drawScene :: forall h eff . STRef h State -> EffWebGL (st :: ST h |eff) Unit
drawScene stRef = do
  s <- readSTRef stRef
  canvasWidth <- getCanvasWidth s.context
  canvasHeight <- getCanvasHeight s.context
  viewport 0 0 canvasWidth canvasHeight
  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

  let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
  setUniformFloats s.bindings.uPMatrix (M.toArray pMatrix)

  setBlending s
  setLightning s

  renderAll 0 s.bindings s.cubes
               
setBlending :: forall eff. State -> EffWebGL eff Unit
setBlending s = do
    blending <- getElementByIdBool "blending"
    if blending
        then do
            alpha <- getElementByIdFloat "alpha"
            blendFunc SRC_ALPHA ONE
            enable BLEND
            disable DEPTH_TEST
            setUniformFloats s.bindings.uAlpha [alpha]
        else do
            disable BLEND
            enable DEPTH_TEST


setLightning :: forall eff. State -> EffWebGL eff Unit
setLightning s = do
  lighting <- getElementByIdBool "lighting"
  setUniformBoolean s.bindings.uUseLighting lighting
  if lighting
    then do
      ar <- getElementByIdFloat "ambientR"
      ag <- getElementByIdFloat "ambientG"
      ab <- getElementByIdFloat "ambientB"
      setUniformFloats s.bindings.uAmbientColor [ar, ag, ab]
      lx <- getElementByIdFloat "lightDirectionX"
      ly <- getElementByIdFloat "lightDirectionY"
      lz <- getElementByIdFloat "lightDirectionZ"
      let v = V.scale (-1.0)
                  $ V.normalize
                    $ V.vec3 lx ly lz
      setUniformFloats s.bindings.uLightingDirection (V.toArray v)
      dr <- getElementByIdFloat "directionalR"
      dg <- getElementByIdFloat "directionalG"
      db <- getElementByIdFloat "directionalB"
      setUniformFloats s.bindings.uDirectionalColor [dr, dg, db]
    else pure unit

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180.0

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180.0*pi

-- * Key handling

handleKeys ::  forall h eff . STRef h State -> EffWebGL (console :: CONSOLE, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  let cubes =
          map
              (\(ObjectContainer obj) ->
                   if null s.currentlyPressedKeys
                   then (ObjectContainer obj)
                   else
                       let z' = case elemIndex 33 s.currentlyPressedKeys of
                                  Just _ ->  obj.z - 0.05
                                  Nothing -> obj.z
                           z'' = case elemIndex 34 s.currentlyPressedKeys of
                               Just _ ->  z' + 0.05
                               Nothing -> z'
                           ySpeed' = case elemIndex 37 s.currentlyPressedKeys of
                                   Just _ ->  obj.ySpeed - 1.0
                                   Nothing -> obj.ySpeed
                           ySpeed'' = case elemIndex 39 s.currentlyPressedKeys of
                                    Just _ ->  ySpeed' + 1.0
                                    Nothing -> ySpeed'
                           xSpeed' = case elemIndex 38 s.currentlyPressedKeys of
                                   Just _ ->  obj.xSpeed - 1.0
                                   Nothing -> obj.xSpeed
                           xSpeed'' = case elemIndex 40 s.currentlyPressedKeys of
                                    Just _ ->  xSpeed' + 1.0
                                    Nothing -> xSpeed'
                       in
                       ObjectContainer
                         (obj{z=z'',ySpeed=ySpeed'',xSpeed=xSpeed''})
              )
              s.cubes

  _ <- writeSTRef stRef (s { cubes = cubes })
--        log (show s.currentlyPressedKeys)
  pure unit

handleKeyD :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyD stRef event = do
  log "handleKeyDown"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  let cp = case elemIndex key s.currentlyPressedKeys of
                  Just _ ->  s.currentlyPressedKeys
                  Nothing -> key : s.currentlyPressedKeys
  _ <- writeSTRef stRef (s {currentlyPressedKeys = cp})
--  log (show s.currentlyPressedKeys)
  pure unit

handleKeyU :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyU stRef event = do
  log "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  case elemIndex key s.currentlyPressedKeys of
    Nothing ->  pure unit
    Just _ -> do
      _ <- writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
      -- log (show s.currentlyPressedKeys)
      pure unit

module Model.Cube where

import Prelude
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Array (delete, elemIndex, (:), null)
import Math (pi)
import Data.Int (toNumber)

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

import Model.Renderable
    
data Model = Cube
    { cubeVertices :: Buffer T.Float32
    , cubeVerticesNormal :: Buffer T.Float32
    , textureCoords :: Buffer T.Float32
    , cubeVertexIndices :: Buffer T.Uint16
    , texture :: WebGLTex
    }

cubeV :: Array Number
cubeV = [
        -- Front face
        -1.0, -1.0,  1.0,
         1.0, -1.0,  1.0,
         1.0,  1.0,  1.0,
        -1.0,  1.0,  1.0,

        -- Back face
        -1.0, -1.0, -1.0,
        -1.0,  1.0, -1.0,
         1.0,  1.0, -1.0,
         1.0, -1.0, -1.0,

        -- Top face
        -1.0,  1.0, -1.0,
        -1.0,  1.0,  1.0,
         1.0,  1.0,  1.0,
         1.0,  1.0, -1.0,

        -- Bottom face
        -1.0, -1.0, -1.0,
         1.0, -1.0, -1.0,
         1.0, -1.0,  1.0,
        -1.0, -1.0,  1.0,

        -- Right face
         1.0, -1.0, -1.0,
         1.0,  1.0, -1.0,
         1.0,  1.0,  1.0,
         1.0, -1.0,  1.0,

        -- Left face
        -1.0, -1.0, -1.0,
        -1.0, -1.0,  1.0,
        -1.0,  1.0,  1.0,
        -1.0,  1.0, -1.0
      ]

vertexNormals :: Array Number
vertexNormals = [
        -- Front face
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,

        -- Back face
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,

        -- Top face
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,

        -- Bottom face
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,

        -- Right face
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,

        -- Left face
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0
      ]

texCoo :: Array Number
texCoo = [
          -- Front face
          0.0, 0.0,
          1.0, 0.0,
          1.0, 1.0,
          0.0, 1.0,

          -- Back face
          1.0, 0.0,
          1.0, 1.0,
          0.0, 1.0,
          0.0, 0.0,

          -- Top face
          0.0, 1.0,
          0.0, 0.0,
          1.0, 0.0,
          1.0, 1.0,

          -- Bottom face
          1.0, 1.0,
          0.0, 1.0,
          0.0, 0.0,
          1.0, 0.0,

          -- Right face
          1.0, 0.0,
          1.0, 1.0,
          0.0, 1.0,
          0.0, 0.0,

          -- Left face
          0.0, 0.0,
          1.0, 0.0,
          1.0, 1.0,
          0.0, 1.0
        ]

cvi :: Array Int
cvi = [
        0, 1, 2,      0, 2, 3,    -- Front face
        4, 5, 6,      4, 6, 7,    -- Back face
        8, 9, 10,     8, 10, 11,  -- Top face
        12, 13, 14,   12, 14, 15, -- Bottom face
        16, 17, 18,   16, 18, 19, -- Right face
        20, 21, 22,   20, 22, 23  -- Left face
      ]

init :: forall eff. WebGLTex -> EffWebGL (eff) Model
init texture = do
  cubeVertices <- makeBufferFloat cubeV
  cubeVerticesNormal <- makeBufferFloat vertexNormals
  
  textureCoords <- makeBufferFloat texCoo
  cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array cvi
                                        
  pure $
       Cube
           { cubeVertices : cubeVertices
           , cubeVerticesNormal : cubeVerticesNormal
           , textureCoords : textureCoords
           , cubeVertexIndices : cubeVertexIndices
           , texture : texture
           }

bindBuffersForTexture :: forall eff. Model -> EffWebGL (eff) Unit
bindBuffersForTexture (Cube s) = do
  bindBuf s.cubeVertexIndices
  drawElements TRIANGLES s.cubeVertexIndices.bufferSize
       
instance cubeRenderable :: Renderable Model where
    render (Cube s) bindings = do
      bindBufAndSetVertexAttr s.cubeVertices bindings.aVertexPosition
      bindBufAndSetVertexAttr s.cubeVerticesNormal bindings.aVertexNormal
      bindBufAndSetVertexAttr s.textureCoords bindings.aTextureCoord
      withTexture2D s.texture 0 bindings.uSampler 0
                        (bindBuffersForTexture (Cube s))

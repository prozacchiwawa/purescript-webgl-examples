module Model.Renderable where

import Prelude
import Graphics.WebGLAll (EffWebGL, WebGLTex, Buffer, WebGLProg, WebGLContext, Float, Uniform, Vec3, Bool, Sampler2D, Mat4, Vec2,
        Attribute, BlendingFactor(ONE, SRC_ALPHA), BufferTarget(ELEMENT_ARRAY_BUFFER), Capacity(DEPTH_TEST, BLEND), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT),
        Mode(TRIANGLES), Shaders(Shaders), TexFilterSpec(MIPMAP), setUniformFloats, setUniformBoolean, enable, disable, blendFunc, drawElements,
        bindBuf, withTexture2D, bindBufAndSetVertexAttr, clear, viewport, getCanvasHeight, getCanvasWidth, requestAnimationFrame, texture2DFor,
        clearColor, makeBuffer, makeBufferFloat, withShaders, runWebGL)
import Data.ArrayBuffer.Types (Uint16, Float32) as T
import Model.MyBindings

class Renderable a where
    render :: forall eff b. a -> {webGLProgram :: WebGLProg | MyBindings} -> EffWebGL (eff) Unit

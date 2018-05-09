module Model.MyBindings where

import Graphics.WebGLAll (EffWebGL, WebGLTex, Buffer, WebGLProg, WebGLContext, Float, Uniform, Vec3, Bool, Sampler2D, Mat4, Vec2,
        Attribute, BlendingFactor(ONE, SRC_ALPHA), BufferTarget(ELEMENT_ARRAY_BUFFER), Capacity(DEPTH_TEST, BLEND), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT),
        Mode(TRIANGLES), Shaders(Shaders), TexFilterSpec(MIPMAP), setUniformFloats, setUniformBoolean, enable, disable, blendFunc, drawElements,
        bindBuf, withTexture2D, bindBufAndSetVertexAttr, clear, viewport, getCanvasHeight, getCanvasWidth, requestAnimationFrame, texture2DFor,
        clearColor, makeBuffer, makeBufferFloat, withShaders, runWebGL)

type MyBindings =
    (aVertexPosition :: Attribute Vec3
    , aVertexNormal :: Attribute Vec3
    , aTextureCoord :: Attribute Vec2
    , uPMatrix :: Uniform Mat4
    , uMVMatrix:: Uniform Mat4
    , uNMatrix:: Uniform Mat4
    , uSampler :: Uniform Sampler2D
    , uUseLighting :: Uniform Bool
    , uAmbientColor :: Uniform Vec3
    , uLightingDirection :: Uniform Vec3
    , uDirectionalColor :: Uniform Vec3
    , uAlpha :: Uniform Float
    )

shaders :: Shaders (Record MyBindings)
shaders = Shaders

  """
      precision mediump float;

      varying vec2 vTextureCoord;
      varying vec3 vLightWeighting;

      uniform float uAlpha;

      uniform sampler2D uSampler;

      void main(void) {
          vec4 textureColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
          gl_FragColor = vec4(textureColor.rgb * vLightWeighting, textureColor.a * uAlpha);
      }
  """

  """
      attribute vec3 aVertexPosition;
      attribute vec3 aVertexNormal;
      attribute vec2 aTextureCoord;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;
      uniform mat3 uNMatrix;

      uniform vec3 uAmbientColor;

      uniform vec3 uLightingDirection;
      uniform vec3 uDirectionalColor;

      uniform bool uUseLighting;

      varying vec2 vTextureCoord;
      varying vec3 vLightWeighting;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
          vTextureCoord = aTextureCoord;

          if (!uUseLighting) {
              vLightWeighting = vec3(1.0, 1.0, 1.0);
          } else {
              vec3 transformedNormal = uNMatrix * aVertexNormal;
              float directionalLightWeighting = max(dot(transformedNormal, uLightingDirection), 0.0);
              vLightWeighting = uAmbientColor + uDirectionalColor * directionalLightWeighting;
          }
      }
  """

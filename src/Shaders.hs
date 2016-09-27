module Shaders
  ( loadProgram
  , vertexShader
  , fragmentShader
  ) where

import Graphics.GL
import Foreign.Storable (Storable, peek, poke)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types
import Foreign.C.String (newCString, peekCString)
import Control.Monad (when)
import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.Marshal.Array (allocaArray0)

loadProgram :: String -> String -> IO GLuint
loadProgram vertShader fragShader = do

  vertexShaderID <- loadShader GL_VERTEX_SHADER vertShader
  fragmentShaderID <- loadShader GL_FRAGMENT_SHADER fragShader

  progID <- glCreateProgram
  putStrLn "Linking program"

  glAttachShader progID vertexShaderID
  glAttachShader progID fragmentShaderID

  glLinkProgram progID
  _ <- checkStatus GL_LINK_STATUS glGetProgramiv glGetProgramInfoLog progID
  glDeleteShader vertexShaderID
  glDeleteShader fragmentShaderID
  return progID

loadShader :: GLenum -> String -> IO GLuint
loadShader shaderTypeFlag code = do

  shaderID <- glCreateShader shaderTypeFlag
  newCode <- newPointer =<< newCString code
  glShaderSource shaderID 1 newCode nullPtr

  putStrLn "Compiling shader..."
  glCompileShader shaderID
  _ <- checkStatus GL_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog shaderID
  return shaderID

checkStatus
  :: GLenum
  -> (GLuint -> GLenum -> Ptr GLint -> IO ())
  -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr CChar -> IO ())
  -> GLuint
  -> IO Bool
checkStatus statusFlag glGet glGetInfoLog componentID = do
  let
    fetch info = withNewPtr (glGet componentID info)
  status <- fmap toBool $ fetch statusFlag
  logLength <- fetch GL_INFO_LOG_LENGTH
  when (logLength > 0) $
    allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
      _ <- glGetInfoLog componentID logLength nullPtr msgPtr
      msg <- peekCString msgPtr
      (if status then putStrLn else fail) msg
  return status

toBool :: GLint -> Bool
toBool 0 = False
toBool _ = True

withNewPtr :: Storable a => (Ptr a -> IO b) -> IO a
withNewPtr f = alloca (\p -> f p >> peek p)

newPointer :: Storable a => a -> IO (Ptr a)
newPointer v = do
  ptr <- malloc
  poke ptr v
  return ptr

fragmentShader :: String
fragmentShader = unlines
  [ "#version 330 core"
  , "out vec3 color;"
  , "void main()"
  , "{"
  , "  color =  vec3(1,0,0);"
  , "}"
  ]

vertexShader :: String
vertexShader = unlines
  [ "#version 330 core"
  , "layout(location = 0) in vec3 vertexPosition_modelspace;"
  , "void main()"
  , "{"
  , "  gl_Position.xyz = vertexPosition_modelspace;"
  , "  gl_Position.w = 1.0;"
  , "}"
  ]

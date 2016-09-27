{-# LANGUAGE LambdaCase #-}

import Graphics.GL
import Bindings.GLFW

import Foreign.C.String (newCString)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (Storable, sizeOf, peek)
import Foreign.Marshal.Alloc (malloc)

import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable (Vector)

import Shaders

main :: IO ()
main = do

  -- Window
  window <- createWindow

  -- Keys
  c'glfwSetInputMode window c'GLFW_STICKY_KEYS c'GL_TRUE

  -- VAO creation
  vertexArrayID <- malloc
  glGenVertexArrays 1 vertexArrayID
  glBindVertexArray =<< peek vertexArrayID

  -- Buffer creation
  vertexBufferID <- malloc
  glGenBuffers 1 vertexBufferID
  glBindBuffer GL_ARRAY_BUFFER =<< peek vertexBufferID

  programID <- loadProgram vertexShader fragmentShader

  -- Triangle
  let points = Vector.fromList [-1,-1,0,1,-1,0,0,1,0] :: Vector Float
      pointsSize = fromIntegral $ sizeOfArray points

  Vector.unsafeWith points $ \ptr ->
    glBufferData
      GL_ARRAY_BUFFER
      pointsSize
      (castPtr ptr)
      GL_STATIC_DRAW

  glClearColor 0 0 0.4 0

  loop window $ do

    glClear GL_COLOR_BUFFER_BIT
    glClear GL_DEPTH_BUFFER_BIT

    glUseProgram programID

    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER =<< peek vertexBufferID
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

    glDrawArrays GL_TRIANGLES 0 3
    glDisableVertexAttribArray 0

    c'glfwSwapBuffers window
    c'glfwPollEvents

loop :: Ptr C'GLFWwindow -> IO a -> IO ()
loop window action = do
  c'glfwGetKey window c'GLFW_KEY_ESCAPE >>= \x ->
    case x /= c'GLFW_PRESS of
      False -> c'glfwTerminate
      True -> c'glfwWindowShouldClose window >>= \y ->
        case y == 0 of
          False -> c'glfwTerminate
          True -> action >> loop window action


createWindow :: IO (Ptr C'GLFWwindow)
createWindow = do

  c'glfwInit >>= \x ->
    case x == 1 of
      True -> return ()
      False -> error "Failed to initializa GLFW"

  c'glfwWindowHint c'GLFW_SAMPLES 4
  c'glfwWindowHint c'GLFW_CONTEXT_VERSION_MAJOR 3
  c'glfwWindowHint c'GLFW_CONTEXT_VERSION_MINOR 3
  c'glfwWindowHint c'GLFW_OPENGL_FORWARD_COMPAT c'GL_TRUE
  c'glfwWindowHint c'GLFW_OPENGL_PROFILE c'GLFW_OPENGL_CORE_PROFILE

  windowName <- newCString "Tutorial 01"
  window <- c'glfwCreateWindow 1024 768 windowName nullPtr nullPtr

  c'glfwMakeContextCurrent window

  return window

sizeOfArray :: Storable a => Vector a -> Int
sizeOfArray v = sizeOf (v Vector.! 0) * Vector.length v

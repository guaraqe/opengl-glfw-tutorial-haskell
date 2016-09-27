import Bindings.GLFW

import Foreign.C.String (newCString)
import Foreign.Ptr (Ptr, nullPtr)

main :: IO ()
main = do

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

  c'glfwSetInputMode window c'GLFW_STICKY_KEYS c'GL_TRUE

  loop window $ do

    c'glfwSwapBuffers window
    c'glfwPollEvents

loop :: Ptr C'GLFWwindow -> IO a -> IO ()
loop window action =
  c'glfwGetKey window c'GLFW_KEY_ESCAPE >>= \press ->
    case press /= c'GLFW_PRESS of
      False -> return ()
      True -> c'glfwWindowShouldClose window >>= \close ->
        case close == 0 of
          False -> return ()
          True -> action >> loop window action

\begin{code}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad
\end{code}

\begin{code}
n :: [Normal3 GLfloat]
n = [ Normal3 (-1.0) 0.0 0.0
    , Normal3 0.0 1.0 0.0
    , Normal3 1.0 0.0 0.0
    , Normal3 0.0 (-1.0) 0.0
    , Normal3 0.0 0.0 1.0
    , Normal3 0.0 0.0 (-1.0) ]
 
faces :: [[Vertex3 GLfloat]]
faces = [ [v !! 0, v !! 1, v !! 2, v !! 3]
        , [v !! 3, v !! 2, v !! 6, v !! 7]
        , [v !! 7, v !! 6, v !! 5, v !! 4]
        , [v !! 4, v !! 5, v !! 1, v !! 0]
        , [v !! 5, v !! 6, v !! 2, v !! 1]
        , [v !! 7, v !! 4, v !! 0, v !! 3] ]
 
v :: [Vertex3 GLfloat]
v = [ Vertex3 (-1) (-1)   1,  Vertex3 (-1) (-1) (-1)
    , Vertex3 (-1)   1  (-1), Vertex3 (-1)   1    1
    , Vertex3   1  (-1)   1,  Vertex3   1  (-1) (-1)
    , Vertex3   1    1  (-1), Vertex3   1    1    1 ]
\end{code}

\begin{code}
drawBox :: IO ()
drawBox = zipWithM_ renderFace n faces
    where
      renderFace norm face = renderPrimitive Quads $ do
                               normal norm
                               mapM_ vertex face
 
display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  drawBox
  swapBuffers
 
initfn :: IO ()
initfn = do 
  diffuse (Light 0) $= Color4 1.0 1.0 1.0 1.0
  position (Light 0) $= Vertex4 1.0 1.0 1.0 0.0
  light (Light 0) $= Enabled
  lighting $= Enabled

  color $ ((Color4 1.0 0.0 0.0 1.0)::Color4 GLfloat)
 
  depthFunc $= Just Lequal
 
  matrixMode $= Projection
  perspective 40.0 1.0 1.0 10.0
  matrixMode $= Modelview 0
  lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  
  translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)
  rotate 60 ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
  rotate (-20) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)
\end{code}

\begin{code}
main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  createWindow "red 3D lighted cube"
  displayCallback $= display
  initfn
  v <- get currentColor
  print v
  mainLoop
\end{code}

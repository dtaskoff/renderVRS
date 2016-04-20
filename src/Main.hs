module Main where

import Graphics.UI.GLUT
import Data.Array.IArray ((!))
import Foreign.Ptr (Ptr)
import Types.Node (Transform)

import Core


main :: IO ()
main = do
  (_progName, args) <- getArgsAndInitialize

  let (path:args') =
        case args of
        [] -> error "Pass path to .vrscene file!"
        _  -> args

  initialWindowSize $= Size 640 480
  _window <- createWindow "RenderVRS"

  matrixMode $= Projection
  loadIdentity
  perspective 78.53982 (3/2) 0.1 1000

  putStrLn "parsing.."
  (vertices, normals, colours, t, n) <- loadNode path
  -- ^ Load pointers to vertices, normals, colours,
  -- linear transformation and number of faces
  putStrLn "parsing done"

  matrixMode $= Modelview 0
  loadIdentity

  let (x:y:z:transform) = 
        case args' of
          ("transform":_) -> toList t
          ("t":_)         -> toList t
          _               -> [0, 0, 20]
          -- ^ If no transformation is supplied, use a default one

  lookAt (Vertex3 x y z) (Vertex3 0 0 0) (Vector3 0 1 0)

  if null transform
  then return ()
  else (newMatrix RowMajor transform :: IO (GLmatrix Double)) >>= multMatrix

  normalize $= Enabled
  shadeModel $= Smooth
  lighting $= Enabled
  light (Light 0) $= Enabled

  colorMaterial $= Just (FrontAndBack, Diffuse)
  materialDiffuse Front $= Color4 0.5 0.5 0.5 1

  displayNode vertices normals colours n
  -- ^ The actual rendering is happening here

  mainLoop


-- | Convert tranformation to a list of translation and
-- a 4x4 OpenGL transformation matrix
toList :: Transform -> [Double]
toList (m, (x, y, z)) = [ x, y, z
                        , x0, x1, x2, 0
                        , y0, y1, y2, 0
                        , z0, z1, z2, 0
                        , 0, 0, 0, 1]
  where (x0, y0, z0) = m ! 0
        (x1, y1, z1) = m ! 1
        (x2, y2, z2) = m ! 2


displayNode :: Ptr Double -> Ptr Double -> Ptr Double -> GLsizei -> IO ()
displayNode vertices normals colours n = do
  displayCallback $= do
    clear [ColorBuffer]
    clientState VertexArray $= Enabled
    clientState NormalArray $= Enabled
    clientState ColorArray $= Enabled

    arrayPointer VertexArray $= VertexArrayDescriptor 3 Double 0 vertices
    arrayPointer NormalArray $= VertexArrayDescriptor 3 Double 0 normals
    arrayPointer ColorArray $= VertexArrayDescriptor 3 Double 0 colours

    putStrLn "drawing.."
    drawArrays Triangles 0 n
    putStrLn "drawing done."

    clientState VertexArray $= Disabled
    clientState NormalArray $= Disabled
    clientState ColorArray $= Disabled
    flush

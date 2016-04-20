module Core where


import Foreign.Marshal.Array (newArray)
import Foreign.Ptr (Ptr)
import Parser (readVRScene)
import Types.VRScene (VRScene(..))
import Types.Node (nodeGeometry, nodeTransform, Transform)
import Types.GeomStaticMesh (GeomStaticMesh(..))
import Data.HashMap.Lazy as HashMap ((!), elems)
import Data.Array.IArray as Array (Array, (!))
import Graphics.UI.GLUT (GLsizei)


loadNode ::
  FilePath -> IO (Ptr Double, Ptr Double, Ptr Double, Transform, GLsizei)
loadNode path = do
  (VRScene nodes meshes) <- readVRScene path

  -- Currenty working with only one node and one geometry
  let (mesh, transformation) = head . elems $
        fmap (\node ->
          (meshes HashMap.! nodeGeometry node, nodeTransform node)) nodes
      
      (vertices', n) = extractVertices mesh

  vertices <- newArray vertices'
  normals <- newArray . fst $ extractNormals mesh
  colours <- newArray . fst $ extractColours mesh

  pure (vertices, normals, colours, transformation, n)

extractVertices, extractNormals, extractColours ::
  GeomStaticMesh -> ([Double], GLsizei)

extractVertices (GeomStaticMesh _ vertices faces _ _ _ _ _ _ _) = 
  extract vertices faces
extractNormals (GeomStaticMesh _ _ _ normals faceNormals _ _ _ _ _) = 
  extract normals faceNormals
extractColours (GeomStaticMesh _ _ _ _ _ [(_,colours,faceColours)] _ _ _ _) = 
  extract colours faceColours

extract :: Array Int (a, a, a) -> [Int] -> ([a], GLsizei)
extract arr (i:is) =
    let (xs, n) = extract arr is
    in (ix:iy:iz:xs, n + 1)
  where (ix, iy, iz) = arr Array.! i
extract _ _ = ([], 0)

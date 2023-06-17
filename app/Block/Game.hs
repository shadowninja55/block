module Block.Game where

import Block.Camera
import Control.Lens
import Control.Monad.State (StateT)
import Data.Foldable (fold)
import Data.IORef
import Data.Ix (inRange)
import Data.Traversable (for)
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable qualified as MV
import Foreign (nullPtr, sizeOf, withArray)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL
import Linear

data Block
  = Air
  | Cobble
  | Dirt
  | Grass
  | Stone

data Chunk = Chunk
  { blocks :: IOVector Block
  , vao :: VertexArrayObject
  , vertices :: BufferObject
  , uv :: BufferObject
  , nVertices :: IORef Int
  }

data Env = Env 
  { cam :: Camera
  , lastFrame :: Float
  , deltaTime :: Float
  }
  deriving Generic

type Game = StateT Env IO

textureIndex :: Block -> V3 Int -> Int
textureIndex block face = case (block, face) of
  (Air, _) -> error "faceTexture: air has no texture"
  (Cobble, _) -> 0
  (Stone, _) -> 1
  (Dirt, _) -> 2
  (Grass, V3 0 -1 0) -> 2
  (Grass, V3 0 1 0) -> 4
  (Grass, _) -> 3

faceVertices :: V3 Int -> [(V3 Float, V2 Float)]
faceVertices = \case
  V3 0 0 1 -> -- front face
    [ (V3 1 1 1, V2 1 0)
    , (V3 0 0 1, V2 0 1)
    , (V3 0 1 1, V2 0 0)
    , (V3 1 1 1, V2 1 0)
    , (V3 1 0 1, V2 1 1)
    , (V3 0 0 1, V2 0 1)
    ]
  V3 0 0 -1 -> -- back face
    [ (V3 1 1 0, V2 1 0)
    , (V3 0 1 0, V2 0 0)
    , (V3 0 0 0, V2 0 1)
    , (V3 1 1 0, V2 1 0)
    , (V3 0 0 0, V2 0 1)
    , (V3 1 0 0, V2 1 1)
    ]
  V3 1 0 0 -> -- right face
    [ (V3 1 1 0, V2 1 0)
    , (V3 1 0 1, V2 0 1)
    , (V3 1 1 1, V2 0 0)
    , (V3 1 1 0, V2 1 0)
    , (V3 1 0 0, V2 1 1)
    , (V3 1 0 1, V2 0 1)
    ]
  V3 -1 0 0 -> -- left face
    [ (V3 0 1 0, V2 1 0)
    , (V3 0 1 1, V2 0 0)
    , (V3 0 0 1, V2 0 1)
    , (V3 0 1 0, V2 1 0)
    , (V3 0 0 1, V2 0 1)
    , (V3 0 0 0, V2 1 1)
    ]
  V3 0 1 0 -> -- top face
    [ (V3 1 1 0, V2 1 1)
    , (V3 0 1 1, V2 0 0)
    , (V3 0 1 0, V2 0 1)
    , (V3 1 1 0, V2 1 1)
    , (V3 1 1 1, V2 1 0)
    , (V3 0 1 1, V2 0 0)
    ]
  V3 0 -1 0 -> -- bottom face
    [ (V3 1 0 0, V2 1 1)
    , (V3 0 0 0, V2 0 1)
    , (V3 0 0 1, V2 0 0)
    , (V3 1 0 1, V2 1 0)
    , (V3 1 0 0, V2 1 1)
    , (V3 0 0 1, V2 0 0)
    ]
  _ -> error "faceVertices: invalid face provided"

chunkVertices :: Chunk -> IO ([V3 Float], [V2 Float])
chunkVertices chunk = unzip . fold <$> for (sequenceA [[0..15], [0..255], [0..15]]) \[x, y, z] -> do
  let 
   pos = V3 x y z
   posf = toEnum @Float . fromEnum <$> pos
  getBlock chunk pos >>= \case
    Air -> pure []
    block -> fold <$> for dirs \dir -> do
      let 
       neighbor = pos + dir
       index = toEnum $ textureIndex block dir
       vertices = faceVertices dir <&> bimap (+ posf) (_x %~ offset index)
      if inRange (V3 0 0 0, V3 15 255 15) neighbor
        then getBlock chunk (pos + dir) <&> \case
          Air -> vertices
          _ -> []
        else pure vertices
 where
  dirs = [V3 0 0 1, V3 0 0 -1, V3 1 0 0, V3 -1 0 0, V3 0 1 0, V3 0 -1 0]
  offset i x = (x + i) / 16

updateVBOs :: Chunk -> IO ()
updateVBOs chunk = do
  (vertices, uvCoords) <- chunkVertices chunk
  let nVertices = length vertices
  writeIORef chunk.nVertices nVertices

  bindVertexArrayObject $= Just chunk.vao

  bindBuffer ArrayBuffer $= Just chunk.vertices
  withArray vertices \ptr -> 
    let size = fromIntegral $ nVertices * sizeOf (head vertices)
    in bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  let vLocation = AttribLocation 0
  vertexAttribPointer vLocation $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  vertexAttribArray vLocation $= Enabled 

  bindBuffer ArrayBuffer $= Just chunk.uv
  withArray uvCoords \ptr -> 
    let size = fromIntegral $ nVertices * sizeOf (head uvCoords)
    in bufferData ArrayBuffer $= (size, ptr, StaticDraw)
  let uvLocation = AttribLocation 1
  vertexAttribPointer uvLocation $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  vertexAttribArray uvLocation $= Enabled 

  bindVertexArrayObject $= Nothing

renderChunk :: Chunk -> IO ()
renderChunk chunk = do
  bindVertexArrayObject $= Just chunk.vao
  nVertices <- fromIntegral <$> readIORef chunk.nVertices
  drawArrays Triangles 0 nVertices
  bindVertexArrayObject $= Nothing

chunkIndex :: V3 Int -> Int
chunkIndex (V3 x y z) = z * 16 * 256 + y * 16 + x

emptyChunk :: IO Chunk
emptyChunk = do
  blocks <- MV.replicate (16 * 256 * 16) Air
  vao <- genObjectName
  vertices <- genObjectName
  uv <- genObjectName
  nVertices <- newIORef 0
  let chunk = Chunk blocks vao vertices uv nVertices
  updateVBOs chunk
  pure chunk

getBlock :: Chunk -> V3 Int -> IO Block
getBlock chunk pos = MV.read chunk.blocks $ chunkIndex pos

setBlock :: Chunk -> V3 Int -> Block -> IO ()
setBlock chunk pos block = MV.write chunk.blocks (chunkIndex pos) block 

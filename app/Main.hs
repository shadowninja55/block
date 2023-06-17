{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Block.Camera
import Block.Game
import Control.Exception (bracketOnError)
import Control.Lens
import Control.Monad
import Control.Monad.State hiding (get)
import Data.ByteString qualified as BS
import Data.Foldable (for_, toList, traverse_)
import Data.Generics.Labels
import Data.Ord (clamp)
import Graphics.GLUtil hiding (setUniform)
import Graphics.Rendering.OpenGL as GL
import Linear as L
import SDL qualified
import SDL.Raw.Event (setRelativeMouseMode)
import System.Exit (exitSuccess)

setUniform :: Uniform a => String -> a -> Game ()
setUniform name value = liftIO do
  Just program <- get currentProgram
  location <- get $ uniformLocation program name
  get errors >>= traverse_ \(Error _ message) -> putStrLn $ "setUniform error: " <> message
  uniform location $= value

shutdown :: SDL.Window -> Game ()
shutdown window = do
  SDL.destroyWindow window
  liftIO exitSuccess

radians :: (Num a, Floating a) => a -> a
radians = (* (pi / 180))

updateDeltaTime :: Game ()
updateDeltaTime = do
  time <- SDL.time
  lastFrame <- use #lastFrame
  #deltaTime .= time - lastFrame
  #lastFrame .= time

onEvent :: SDL.EventPayload -> Game ()
onEvent = \case
  SDL.KeyboardEvent (SDL.KeyboardEventData (Just window) SDL.Pressed _ sym) -> case sym.keysymScancode of
    SDL.ScancodeEscape -> shutdown window
    SDL.ScancodeEquals -> polygonMode $~ \case
      (Fill, Fill) -> (Line, Line)
      (Line, Line) -> (Fill, Fill)
    _ -> pure ()
  SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ _ motion) -> 
    moveMouse $ toEnum . fromEnum <$> motion
  SDL.WindowClosedEvent (SDL.WindowClosedEventData window) -> shutdown window
  SDL.WindowResizedEvent (SDL.WindowResizedEventData _ (V2 x y)) ->
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral x) (fromIntegral y))
  _ -> pure ()

moveMouse :: V2 Float -> Game ()
moveMouse (V2 x y) = do
  #cam . #yaw += sens * x
  #cam . #pitch -= sens * y
  #cam . #pitch %= clamp (-89, 89)
  Camera {yaw, pitch} <- use #cam
  let 
   dir = V3 
     (cos (radians yaw) * cos (radians pitch))
     (sin $ radians pitch)
     (sin (radians yaw) * cos (radians pitch))
  #cam . #front .= L.normalize dir
 where
  sens = 0.1

processInput :: Game ()
processInput = do
  keys <- SDL.getKeyboardState 
  let held key = when (keys key)
  Camera {front = camFront, up = camUp} <- use #cam
  deltaTime <- use #deltaTime
  let camSpeed = pure deltaTime * if keys SDL.ScancodeLCtrl then 7.5 else 5
  held SDL.ScancodeW $ 
    #cam . #pos += camSpeed * L.normalize (camFront * V3 1 0 1)
  held SDL.ScancodeS $ 
    #cam . #pos -= camSpeed * L.normalize (camFront * V3 1 0 1)
  held SDL.ScancodeA $ 
    #cam . #pos -= camSpeed * L.normalize (camFront `L.cross` camUp)
  held SDL.ScancodeD $ 
    #cam . #pos += camSpeed * L.normalize (camFront `L.cross` camUp)
  held SDL.ScancodeSpace $
    #cam . #pos += camSpeed * V3 0 1 0
  held SDL.ScancodeLShift $
    #cam . #pos -= camSpeed * V3 0 1 0

toMat4 :: M44 Float -> Game (GLmatrix Float)
toMat4 = liftIO . newMatrix RowMajor . foldMap toList

gameLoop :: SDL.Window -> Game ()
gameLoop window = do
  chunk <- liftIO emptyChunk
  liftIO do
    for_ (sequenceA [[0..15], [0..1], [0..15]]) \[x, y, z] ->
      setBlock chunk (V3 x y z) Cobble
    for_ (liftA2 (,) [0..15] [0..15]) \(x, z) ->
      setBlock chunk (V3 x 2 z) Dirt
    for_ (liftA2 (,) [0..15] [0..15]) \(x, z) ->
      setBlock chunk (V3 x 3 z) Grass
    updateVBOs chunk
  
  forever do 
    updateDeltaTime
    SDL.pollEvents >>= traverse_ (onEvent . SDL.eventPayload)
    processInput
    GL.clearColor $= Color4 0.45 0.7 1 1
    liftIO $ GL.clear [ColorBuffer, DepthBuffer]
    Camera {pos = camPos, front = camFront, up = camUp} <- use #cam
    V2 width height <- fmap (toEnum . fromEnum) <$> get (SDL.windowSize window)
    model <- toMat4 $ L.mkTransformation (Quaternion 1 zero) zero
    view <- toMat4 $ L.lookAt camPos (camPos + camFront) camUp
    projection <- toMat4 $ L.perspective (radians 90) (width / height) 0.1 100
    setUniform "model" model
    setUniform "view" view
    setUniform "projection" projection
    liftIO $ renderChunk chunk
    SDL.glSwapWindow window

initWindow :: IO SDL.Window
initWindow = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "purecraft" windowConfig
  SDL.glCreateContext window
  depthFunc $= Just Less
  frontFace $= CW
  cullFace $= Just Back
  setRelativeMouseMode True
  SDL.swapInterval $= SDL.ImmediateUpdates
  pure window
 where
  windowGraphicsContext = SDL.defaultOpenGL
    & #glProfile .~ SDL.Core SDL.Normal 4 6
  windowConfig = SDL.defaultWindow
    & #windowInitialSize .~ V2 800 800
    & #windowGraphicsContext .~ SDL.OpenGLContext windowGraphicsContext

loadShaders :: [(ShaderType, FilePath)] -> IO Program
loadShaders info = createProgram `bracketOnError` deleteObjectName $ \program -> do
  for_ info \(shType, path) -> createShader shType `bracketOnError` deleteObjectName $ \shader -> do
    source <- BS.readFile path
    shaderSourceBS shader $= source
    checked compileShader compileStatus shaderInfoLog "compile" shader
    attachShader program shader
  checked linkProgram linkStatus programInfoLog "link" program
  pure program
 where
  checked action getStatus getInfoLog message object = do
    action object
    ok <- get $ getStatus object
    unless ok do
      infoLog <- get $ getInfoLog object
      fail $ message <> " log: " <> infoLog

loadAssets :: IO ()
loadAssets = do
  tex <- readTexture "atlas.png" <&> \case
    Right tex -> tex
    Left err -> error err
  textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
  generateMipmap' Texture2D
  texture2DWrap $= (Repeated, Repeat)
  texture Texture2D $= Enabled
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just tex

  program <- loadShaders 
    [ (VertexShader, "shader.vert")
    , (FragmentShader, "shader.frag")
    ]
  currentProgram $= Just program

main :: IO ()
main = do
  window <- initWindow
  loadAssets
  time <- SDL.time
  let 
   env = Env 
     { cam = Camera (V3 0 0 3) (V3 0 0 -1) (V3 0 1 0) -90 0
     , lastFrame = time
     , deltaTime = 0
     }
  runStateT (gameLoop window) env
  SDL.destroyWindow window

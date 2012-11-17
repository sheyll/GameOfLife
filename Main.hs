module Main (main) where
import Data.IORef
import Graphics.UI.GLUT.Initialization 
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.Rendering.OpenGL.GL.PerFragment
import Graphics.UI.GLUT.Window
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.Colors
import Graphics.Rendering.OpenGL.GLU.Matrix
import Data.Time.Clock
import GameOfLife

main :: IO ()
main = 
    do   
      (_, args) <- initialiseScreen
      w <- createWindow "Game Of Life"
      let brdArg = case args of
                     []     -> 10
                     nStr:_ -> read nStr
      initialBoard <- randBoard creatures brdArg
      -- let initialBoard = head example_boards
      ref <- newIORef (initialBoard, (0.0::GLfloat))
      idleCallback $= Just (modifyBoard ref w)
      displayCallback $= render ref
      reshapeCallback $= Just initialiseRendering
      keyboardMouseCallback $=  Just (addObject ref)
      mainLoop

initialiseScreen =
    do
      initialDisplayCapabilities $= [ With  DisplayRGBA,
                                      Where DisplayDepth IsAtLeast 16,
                                      With  DisplayDouble ]
      initialDisplayMode         $= [ RGBAMode, 
                                      DoubleBuffered]
      getArgsAndInitialize
   
farCP,nearCP,wh,hh,dh :: GLdouble
farCP  = nearCP + 2 * (fromIntegral sdepth)
nearCP = 150
wh = (fromIntegral swidth)/2
hh = (fromIntegral sheight)/2     
dh = (fromIntegral sdepth)/2

rotAxis :: Vector3 GLfloat
rotAxis = Vector3 0 1 0

initialiseRendering (Size w h) =
    do
      putStrLn $ "Initialize Rendering: " ++ (show w) ++ " " ++ (show h)
      drMode <- get directRendering
      putStrLn $ "Direct rendering mode: " ++ show drMode
      viewport $= (Position 0 0, Size w h)
      clearColor $= clearCol
      matrixMode $= Projection
      loadIdentity
      frustum ((-wh)*1.7) (wh*1.7) ((-hh)*1.7) (hh*1.7) nearCP farCP
      matrixMode $= Modelview 0
      loadIdentity
      drawBuffer $= BackBuffers
      shadeModel $= Smooth
      pointSmooth $= Enabled
      blend $= Enabled
      blendEquation $= FuncAdd
      blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

render ref = 
    do
      (board, angle) <- readIORef ref
      newBoard <- drawGameOfLife board angle      
      swapBuffers
      next_angle <- calcNextAngle angle
      writeIORef ref (newBoard, next_angle)

calcNextAngle :: GLfloat -> IO GLfloat
calcNextAngle angle = do currentSeconds <- (return . 
                                            (`mod` 36000) . 
                                            round . 
                                            toRational . 
                                            (*1000) .
                                            utctDayTime ) =<< getCurrentTime 
                         return $ (fromIntegral currentSeconds) / 100.0

addObject brdRef (MouseButton _) Down mod (Position x y) =
    do
      pos <- randPos
      modifyIORef brdRef 
                      (\(board,angle) ->
                           let src = (case ((shift mod),(ctrl mod)) of 
                                        (Up, Down) -> (stones !! 1)
                                        (Up, Up) -> (creatures !! 2)
                                        (Down, Up) -> (creatures !! 0))
                           in ((translateBrd pos src `joinBoards` board), angle))
addObject _ _ _ _ _ = return ()

modifyBoard ref win =
    do
      postRedisplay (Just win)

drawGameOfLife :: Board -> GLfloat -> IO Board
drawGameOfLife oldBoard angle = 
    do 
      GL.clear [GL.ColorBuffer, GL.DepthBuffer]
      let (childs, deaths, survs, newBoard) = update_life oldBoard 
      loadIdentity
      translate (Vector3 0 0 (-nearCP - dh - dh))
      rotate angle rotAxis
      translate (Vector3 (-wh) (-hh) (-dh))
      pointSize $= 10
      mapM_  (\d' -> renderPrimitive LineLoop
                       (do
                         let dh :: GLdouble
                             dh = fromIntegral d'
                         color white
                         vertex (Vertex3 0 0 dh)
                         vertex (Vertex3 (fromIntegral swidth) 0 dh)
                         vertex (Vertex3 (fromIntegral swidth) (fromIntegral sheight) dh)
                         vertex (Vertex3 0 (fromIntegral sheight) dh)
                       )) [0..sdepth]
      pointSize $= 2
      drawBoard dCol deaths
      pointSize $= 1
      drawBoard sCol survs
      pointSize $= 4
      drawBoard cCol childs
      return newBoard
    where
      drawBoard c board = renderPrimitive Points
                          (mapM_ 
                           (\(x,y,z) -> 
                                do
                                  let coord :: Vertex3 GLfloat
                                      coord = (Vertex3 (fromIntegral x) (fromIntegral y) (fromIntegral z))
                                  color c
                                  vertex coord)
                           (toList board))
                                   

sCol, dCol, cCol :: Color3 GLfloat
sCol  = Color3 1 1 1
dCol   = Color3 0.25 0.01 0.1
cCol    = Color3 1 0.8 0.4

white,red,blue,green :: Color4 GLfloat
white  = Color4 1 1 1 0.05
red  = Color4 1 0 0 0.25
blue  = Color4 0 0 1 0.25
green  = Color4 0 1 0 0.25

backGrnd :: Color4 GLfloat
backGrnd = Color4 0.25 0.25 0.25 0.015625

clearCol :: Color4 GLclampf
clearCol = Color4 0 0 0 1
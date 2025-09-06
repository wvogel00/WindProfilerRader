{-# LANGUAGE OverloadedStrings #-}

module DrawWPR where

import WPR.Binary 
import WPR.Difinition hiding(Pos)
import System.Process (callCommand)
import System.Directory (removeFile)

data XYTuple = XY Float Float deriving Eq
type Pos  = XYTuple
type Size = XYTuple

instance Show XYTuple where
  show (XY x y) = show x ++ "," ++ show y

instance Num XYTuple where
  (XY x1 y1) + (XY x2 y2) = XY (x1+x2) (y1+y2)
  (XY x1 y1) * (XY x2 y2) = XY (x1*x2) (y1*y2)
  (XY x1 y1) - (XY x2 y2) = XY (x1-x2) (y1-y2)
  negate (XY x y) = XY (-x) (-y)
  abs (XY x y) = XY (abs x) (abs y)
  signum (XY x y) = undefined
  fromInteger = undefined

data Color = RED | LIGHT_RED | DARK_RED
           | BLUE | LIGHT_BLUE | DARK_BLUE
           | GREEN | LIGHT_GREEN | DARK_GREEN
           | YELLOW | LIGHT_YELLOW | DARK_YELLOW
           | WHITE deriving Eq

instance Show Color where
  show RED = "red"
  show LIGHT_RED = "light-red"
  show DARK_RED = "dark-red"
  show BLUE = "blue"
  show LIGHT_BLUE = "light-blue"
  show DARK_BLUE = "dark-blue"
  show GREEN = "green"
  show LIGHT_GREEN = "light-green"
  show DARK_GREEN = "dark-green"
  show YELLOW = "yellow"
  show LIGHT_YELLOW = "yellow"
  show DARK_YELLOW = "dark-yellow"
  show WHITE = "white"



samplePNG = "hoge.png"
scriptFile = "wpr.gp"
screenSize = XY 1200 900
squareSize = XY (1/6) 300 -- 矢印描画の幅・高

scriptHeader = unlines [ "set terminal pngcairo size " ++ show screenSize 
                       , "set terminal qt font \"Helvetica,12\""
                       , "set output '"++ samplePNG ++ "'"
                       , "set xrange [21:25] reverse"
                       , "set yrange [0:10000]"
                       ]

drawWPR :: FilePath -> IO ()
drawWPR f = do
  b4 <- analyze f
  let script' = unlines . concat . take 100 $ map makeDrawCMD . zip [1..] $ measurement b4
  let script  = scriptHeader ++ script' ++ "plot 10\nexit\nexit\n"
  print "hogehoge"
  writeFile scriptFile script
  putStrLn $ script
  callCommand $ "gnuplot " ++ scriptFile


makeDrawCMD :: (Int, Measurement) -> [String]
makeDrawCMD (i, m@(Measurement d _ _ _ ws)) = map (makeArrowCMD d) $ zip [i*100..] ws

--MeasuredWind = MeasuredWind {mwHeight :: Height, mwWind :: Wind, mwSN :: SN}
makeArrowCMD :: Date -> (Int, MeasuredWind) -> String
makeArrowCMD date (i,m) = init $ unlines [drawArrow, drawSquare] where
  xAxis = fromIntegral  (hour date) + fromIntegral (minute date)/60 
  (s,w) = ( sWind $ mwWind m, wWind $ mwWind m)
  drawArrow = setArrow i  (XY xAxis (mwHeight m)) squareSize $ windDir s w 
  drawSquare = setSquare (i+1) (XY xAxis (mwHeight m)) squareSize $ getVWindColor (mwWind m)

type Degree = Float
data WindDirection = Calm | Deg360 Degree deriving (Eq, Show)

windDir :: SouthWind -> WestWind -> WindDirection
windDir (S s) (W w)
  | s == 0 && w == 0 = Calm
  | s == 0           = Deg360 $ 180 + 90 * signum w
  |           w == 0 = Deg360 $ 270 + 90 * signum s -- 南風=180, 北風=360
  | s > 0  && w > 0  = Deg360 $ atan (abs w/abs s) * 2/pi * 180
  | s < 0  && w > 0  = Deg360 $ atan (abs s/abs w) * 2/pi * 180 + 90
  | s < 0  && w < 0  = Deg360 $ atan (abs w/abs s) * 2/pi * 180 + 180
  | s > 0  && w < 0  = Deg360 $ atan (abs s/abs w) * 2/pi * 180 + 270


getVWindColor (Wind _ _ (V v))
  | v == 0     = WHITE
  | v > 200    = RED
  | v > 5      = LIGHT_RED
  | v > 2.5    = YELLOW
  | v > 0      = LIGHT_YELLOW 
  | v < (-200) = DARK_BLUE
  | v < (  -5) = BLUE
  | v < (-2.5) = LIGHT_BLUE
  | v < 0      = GREEN

setArrow :: Int -> Pos -> Size -> WindDirection -> String
setArrow i p s Calm = []
setArrow i p@(XY xc yc) s@(XY sx sy) (Deg360 d) = "set arrow " ++ show i ++ 
                                      " from " ++ show startPos ++ 
                                      "  to " ++ show endPos where
                                        d' = d/180*pi
                                        startPos = XY  (xc+sx/2*sin d') (yc+sy/2*cos d')
                                        endPos = XY (xc-sx/2*sin d') (yc-sy/2*cos d')

setSquare :: Int -> Pos -> Size -> Color -> String
setSquare i p s c = "set object " ++ show i ++" rect from " ++ show (p - s*XY 0.5 0.5) ++
                    " to "++ show (p + s*XY 0.5 0.5) ++
                    " fc rgb '" ++ show c ++
                    "' fs solid noborder"

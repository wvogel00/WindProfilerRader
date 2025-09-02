{-# LANGUAGE OverloadedStrings #-}

module WPR.Difinition where

import Data.Binary.Get
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)

data Date = Date { year   :: Int
                 , month  :: Int
                 , day    :: Int
                 , hour   :: Int
                 , minute :: Int
                 , second :: Int
                 } deriving Eq

type Pos = (Latitude, Longtitude)
type Latitude = Float
type Longtitude = Float
type Height = Float

instance Show Date where
  show (Date 0 0 0 h m s) = (showDigitN 2 h) ++ ":" ++ (showDigitN 2 m) ++ ":" ++ (showDigitN 2 s)
  show (Date y m d 0 0 0) = (showDigitN 2 y) ++ "/" ++ (showDigitN 2 m) ++ "/" ++ (showDigitN 2 d)
  show (Date y m d h mt s) = show (Date y m d 0 0 0) ++" " ++ show (Date 0 0 0 h mt s)

showDigitN n v = replicate (n - length (show v)) '0' ++ show v
showfDigitN :: Int -> Int -> Float -> String
showfDigitN i j v = showN i iv ++ "." ++ showN j fv where
  showN :: Int -> String -> String
  showN n v = take n $ (replicate (n - length v) '0') ++ v
  (iv, fv) = divide '.' $ show v
  divide c v = divide' c ([], show v)
  divide' c (xs,[]) = (xs, [])
  divide' c (xs,y:ys) = if y == c then (reverse xs,ys) else divide' c (y:xs, ys)


data Block1 = Block1 { len :: Int
                     , masterTable :: Int
                     , madeBy :: (Int ,Int)
                     , updateNo :: Int
                     , notForall :: Int
                     , vertRef :: Int
                     , category :: (Int, Int)
                     , ver :: (Int ,Int)
                     , madedate :: Date
                     } deriving Eq

instance Show Block1 where
  show b = "----------------------------------------------------------\n" ++
           "　　　　　　　　　　　Block1長: " ++ show (len b) ++ "\n" ++
           "　　　　　　　　BUFRマスター表: " ++ show (masterTable b) ++ "\n" ++
           "　　　（作成中枢，作成副中枢）: " ++ show (madeBy b) ++ "\n" ++
           "　　　　　　　　　更新一連番号: " ++ show (updateNo b) ++ "\n" ++
           "　　　　　　　任意節を含まない: " ++  show (notForall b) ++ "\n" ++
           "　　鉛直観測資料(衛星を除く=2): " ++ show (vertRef b) ++ "\n" ++
           "　資料副カテゴリ（国際，地域）: " ++ show (category b) ++ "\n" ++
           "バージョン（マスタ，ローカル）: " ++ show (ver b) ++ "\n" ++
           "　　　　　　　　　　　　　日付: " ++ show (madedate b) ++ "\n" ++
           "----------------------------------------------------------\n" 

data Block3 = Block3 { b3Len         :: Int
                     , b3Keep        :: Int
                     , stationN      :: Int
                     , compress      :: Int
                     , wmoBlockID    :: Int
                     , wmoPointID    :: Int
                     , b3Pos         :: Pos
                     , antHeight     :: Height
                     , device        :: Int
                     , latencyN      :: Int
                     , repeatN       :: Int
                     , measDate      :: Date
                     , timeID        :: Int
                     , timeSpan      :: Int
                     , latencyN2     :: Int
                     , repeatN2      :: Int
                     , height        :: Height
                     , localRefWidth :: Int
                     , qualityInfo   :: Int
                     , measWind      :: Wind
                     , sn            :: SN
                     }
instance Show Block3 where
  show b = "----------------------------------------------------------\n" ++
           "　　　　　　ブロック長: " ++ show (b3Len b) ++ "\n" ++
           "　　　　　　　　　保留: " ++ show (b3Keep b) ++ "\n" ++
           "　　　　　　　観測局数: " ++ show (stationN b) ++ "\n" ++
           "　　　観測資料・非圧縮: " ++ show (compress b) ++ "\n" ++
           "　　ＷＭＯブロック番号: " ++ show (wmoBlockID b) ++ "\n" ++
           "　　　　ＷＭＯ地点番号: " ++ show (wmoPointID b) ++ "\n" ++
           "　　　　（緯度，経度）: " ++ show (b3Pos b) ++ "\n" ++
           "　　　アンテナ海抜高度: " ++ show (antHeight b) ++ "\n" ++
           "　　　　　　　使用測器: " ++ show (device b) ++ "\n" ++
           "　　　　　　　遅延反復: " ++ show (latencyN b) ++ "\n" ++
           "　　　　　　　反復回数: " ++ show (repeatN b) ++ "\n" ++
           "　　　　　　　測定日時: " ++ show (measDate b) ++ "\n" ++
           "　　　　　　　時間特定: " ++ show (timeID b) ++ "\n" ++
           "　　　　　　　　　期間: " ++ show (timeSpan b) ++ "\n" ++
           "　　　　　　　遅延反復: " ++ show (latencyN2 b) ++ "\n" ++
           "　　　　　　　反復回数: " ++ show (repeatN2 b) ++ "\n" ++
           "観測点高(アンテナ基準): " ++ show (height b) ++ "\n" ++
           "　ローカル記述子資料幅: " ++ show (localRefWidth b) ++ "\n" ++
           "　風（東+・北+・鉛直）: " ++ show (measWind b) ++ "\n" ++
           "　　　　　　　　Ｓ／Ｎ: " ++ show (sn b) ++ "\n"

data Block4 = Block4 { b4Len :: Int
                     , b4Keep :: Int
                     , _wmoBlockID :: Int
                     , _wmoPointID :: Int
                     , _pos :: Pos
                     , _antHeight :: Height
                     , _device :: Int
                     , _repeatN :: Int
                     , measurement :: [Measurement]
                     } deriving Eq
data Measurement = Measurement { mDate         :: Date
                               , mTimeID       :: Int
                               , mSpan         :: Int
                               , mRepeatN      :: Int
                               , measuredWinds :: [MeasuredWind]
                               } deriving Eq

instance Show Block4 where
   show b = "----------------------------------------------------------\n" ++
           "　　　　　　ブロック長: " ++ show (b4Len b) ++ "\n" ++ 
           "保留: " ++ show (b4Keep b) ++ "\n" ++
           "ＷＭＯブロック番号: " ++ show (_wmoBlockID b) ++ "\n" ++
           "ＷＭＯ地点番号: " ++ show (_wmoPointID b) ++ "\n" ++
           "（緯度，経度）: " ++ show (_pos b) ++ "\n" ++
           "アンテナ海抜高度: " ++ show (_antHeight b) ++ "\n" ++
           "使用測器(ＷＰＲ＝６): " ++ show (_device b) ++ "\n" ++
           "　　　反復回数（Ｘ）: " ++ show (_repeatN b) ++ "\n" ++
           concat (map show $ measurement b) ++
           "                 ------------------------------------------\n"

instance Show Measurement where
   show m = "　　　測定日時: " ++ show (mDate m) ++ "\n" ++
            "　　　時間特定: " ++ show (mTimeID m) ++ "\n" ++
            "　　　　　期間: " ++ show (mSpan m) ++ "\n" ++
            "反復回数（Ｙ）: " ++ show (mRepeatN m) ++ "\n" ++
            "　　　　測定値: " ++ concat (map show $ measuredWinds m) ++ "\n" ++
           "                 ------------------------------------------\n"


           
-- 北向き・東向き，および鉛直上向きの風向を正とする
data SouthWind = S Float deriving Eq
data  WestWind = W Float deriving Eq
data  VertWind = V Float deriving Eq
data      Wind = Wind SouthWind WestWind VertWind deriving Eq
data MeasuredWind = MeasuredWind {mwHeight :: Height, mwWind :: Wind, mwSN :: SN} deriving Eq
type SN = Float
data IUPC = IUPC Int deriving (Show, Eq)
instance Show MeasuredWind where
  show mw = "　　観測点高: " ++ show (mwHeight mw) ++ "\n" ++
            "　　（西風，南風，鉛直風） : " ++ show (mwWind mw) ++ 
            "　　S/N: " ++ show (mwSN mw) ++ "\n"
instance Show Wind where
  show (Wind (S s) (W w) (V v)) = "("  ++ showfDigitN 3 1 s ++ 
                                  ", " ++ showfDigitN 3 1 w ++
                                  ", " ++ showfDigitN 3 1 v ++ 
                                  ")"
-- 風向resolution
type WDResolution = Int
type WindDirection = Int
wdRes = 16

areaName :: IUPC -> [Text]
areaName (IUPC 41) = ["留萌", "帯広", "室蘭"]
areaName (IUPC 42) = ["宮古","酒田", "仙台", "若松"]
areaName (IUPC 43) = ["熊谷", "水戸", "勝浦"]
areaName (IUPC 44) = ["高田", "河口湖", "静岡"]
areaName (IUPC 45) = ["名古屋", "尾鷲", "福井"]
areaName (IUPC 46) = ["浜田", "高松", "高知", "清水"]
areaName (IUPC 47) = ["熊本", "大分", "延岡"]
areaName (IUPC 48) = ["厳原", "平戸", "屋久島", "与那国島"]
areaName (IUPC 49) = ["八丈島", "美浜", "鳥取"]
areaName (IUPC 50) = ["市來", "名瀬", "南大東島"]

-- 0:微風，16:南風 4: 西風
calc2DDirection :: SouthWind -> WestWind -> WindDirection
calc2DDirection (S s) (W w)
  | abs s < 0.3 && abs w < 0.3 = 1000
  | s == 0 =  (8 - 4 * (floor $ signum w))
  | w == 0 =  4 - 4 * (floor $ signum s)
  | s > 0 && w > 0 =  getWD $ atan (w/s)/pi*180
  | s < 0 && w > 0 =  getWD $  90+atan ((abs s)/w)/pi*180
  | s < 0 && w < 0 =  getWD $ 180+atan ((abs w)/ (abs s))/pi*180
  | s > 0 && w < 0 =  getWD $ 270+atan ((abs s)/w)/pi*180

getDeg a b = atan (b/a)/pi*180
getWD deg = fst . head . filter (f deg) $ zip (16:[1..16]) $ [0, 360/wdRes .. 360]++[360] where
  f d (i, d') = d'-180/wdRes < d && d < d'+180/wdRes

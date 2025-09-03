{-# LANGUAGE OverloadedStrings #-}

module WPR.BUFR where

import qualified WPR.BUFRRef as BR 
import qualified Data.Text as T
import Data.Text (Text)

data TableRefID = FXY Int Int Int deriving Eq
data TableClass = BUFR | CREX deriving (Eq, Show)
data BUFRFlag = BUFRFlag Unit Scale RefV InfoBit deriving (Eq, Show)
data Unit = M | CM | KG | Pa | K | DEG | RAD | M_S | PA_S | DB | NA deriving (Eq, Show)
data Device = PaMeter | LightTheodolite | RadioTheodolite | Rader | VLF | Rollan | WPR | Satellite | RASS | Sodar | KEEP | Pa_Stop | MissingData | NoHit deriving (Eq, Show, Enum)

deviceName i = let xs = [PaMeter .. MissingData]
               in if i >= length xs then NoHit else xs !! i
type Scale = Int
type RefV = Int
type InfoBit = Int

type WMOBlockID = Int
type WMOPointID = Int

readWMOTable =  map (f . words) . lines <$> readFile "WMODB.txt" where 
  f :: [String] -> (WMOPointID, Text)
  f [v,name] = (read v, T.pack name)
  f s = undefined

wmoPointName :: WMOBlockID -> WMOPointID -> [(WMOPointID, Text)] -> Text
wmoPointName b p wmoDB = case lookup (b*1000+p) wmoDB of
                            Nothing   -> T.concat ["該当なし ", T.pack $ show p]
                            Just name -> name 


instance Show TableRefID where
  show (FXY 0 2 y) | y == 001 = "観測所の種類"
                   | y == 002 = "風観測機器の種類"
                   | y == 003 = "使用測器の種類"
                   | y == 004 = "蒸発量を測定した測器または蒸発散料を封じた穀物の種類"
                   | y == 005 = "温度の観測精度"
                   | y == 007 = "水位測器センサーの種類"
                   | y == 011 = "ラジオゾンデの種類 "
                   | y == 012 = "ラジオゾンデの計算法"
                   | y == 013 = "日射及び赤外放射の補正"
                   | y == 014 = "トラッキング法／システムの状態"
                   | y == 015 = "ラジオゾンデの構成 "
                   | y == 016 = "ラジオゾンデ飛揚器材"
                   | y == 017 = "Correction algorithms for humidity measurements"
                   | y == 019 = "衛星観測機器"
                   | y == 020 = "衛星の分類"
                   | y == 021 = "処理に用いた衛星観測機器*"
                   | y == 022 = "衛星資料処理技法"
                   | y == 023 = "風の測定方法（衛星）"
                   | y == 024 = "平均湿度計算法"
                   | y == 025 = "計算に用いた衛星チャンネル"
                   | y == 026 = "クロストラック分解能"
                   | y == 027 = "アロングトラック分解能"
                   | y == 028 = "天底（nadir）におけるセグメントのＸ軸方向の大きさ"
                   | y == 029 = "天底におけるセグメントのＹ軸方向の大きさ"
                   | y == 030 = "海流の測定方法"
                   | y == 031 = "海流測定の期間及び時刻"
                   | y == 032 = "数値化の指示符"
                   | y == 033 = "塩分／深度測定法"
                   | y == 034 = "ドローグの種類"
                   | y == 035 = "ケーブルの長さ[m]"
                   | y == 036 = "ブイの種類"
                   | y == 037 = "潮位の観測方法"
                   | y == 038 = "海面水温／塩分の観測方法"
                   | y == 039 = "湿球温度の観測方法"
                   | y == 040 = "海流測定におけるプラットフォームの速度及び動揺の除去法"
                   | y == 041 = "総観規模の擾乱の解析法"
                   | y == 042 = "海面流の流速の指示符"
                   | y == 044 = "波浪スペクトル資料の計算法の指示符"
                   | y == 045 = "プラットフォ－ムの種類の指示符"
                   | y == 046 = "波浪計測機器"
                   | y == 047 = "Deep-Ocean tsunameter platform type/manufacturer"
                   | y == 048 = "衛星のセンサーの指示符"
                   | y == 049 = "使用した静止衛星資料の処理法"
                   | y == 050 = "使用した静止衛星観測チャンネル"
                   | y == 051 = "最高・最低気温の観測方法の指示符"
                   | y == 052 = "使用した静止衛星画像チャンネル"
                   | y == 053 = "ＧＯＥＳ－I/M輝度温度の特性"
                   | y == 054 = "ＧＯＥＳ－I/M観測パラメータの特性"
                   | y == 055 = "静止衛星観測の統計パラメータ"
                   | y == 056 = "静止衛星観測の精度の統計"
                   | y == 057 = "ＧＯＥＳ－I/M観測の第１推定情報の作成方法"
                   | y == 058 = "ＧＯＥＳ－I/M観測の第１推定情報の有効期間"
                   | y == 059 = "ＧＯＥＳ－I/M観測の解析情報の作成方法"
                   | y == 060 = "ＧＯＥＳ－I/M観測の地上情報の作成方法"
                   | y == 061 = "航空機の航法システム"
                   | y == 062 = "航空機資料通報システムの種類"
                   | y == 063 = "航空機の横転角[deg]"
                   | y == 064 = "航空機の横転角の品質"
                   | y == 065 = "ＡＣＡＲＳ地上受信局"
                   | y == 066 = "ラジオゾンデ地上受信システム"
                   | y == 067 = "ラジオゾンデ運用周波数"
                   | y == 070 = "緯度・経度の基となった位置"
                   | y == 071 = "分光学的波長"
                   | y == 072 = "Spectorographic Width"
                   | y == 080 = "気球製造業者"
                   | y == 081 = "気球の種類"
                   | y == 082 = "気球の重量"
                   | y == 083 = "気球シェルターの種類"
                   | y == 084 = "気球に充填されたガスの種類"
                   | y == 085 = "気球に充填されたガスの量"
                   | y == 086 = "気球とゾンデ間の長さ"
                   | y == 091 = "エントリーセンサー 4/20mA"
                   | y == 095 = "気圧センサーの種類"
                   | y == 096 = "温度センサーの種類"
                   | y == 097 = "湿度センサーの種類"
                   | y == 099 = "偏光"
                   | y == 095 = ""

  show (FXY 0 4 y) | y == 001 = "年"
                   | y == 002 = "月"
                   | y == 003 = "日"
                   | y == 004 = "時"
                   | y == 005 = "分"
                   | y == 006 = "秒"
                   | y == 007 = "1分中の秒"
                   | y == 011 = "時間増分（年）"
                   | y == 012 = "時間増分（月）"
                   | y == 013 = "時間増分（日）"
                   | y == 014 = "時間増分（時）"
                   | y == 015 = "時間増分（分）"
                   | y == 016 = "時間増分（秒）"
                   | y == 017 = "積算または極値の参照時間（分）"
                   | y == 021 = "期間または時間変位(年)"
                   | y == 022 = "期間または時間変位(月)"
                   | y == 023 = "期間または時間変位(日)"
                   | y == 024 = "期間または時間変位(時)"
                   | y == 025 = "期間または時間変位(分)"
                   | y == 026 = "期間または時間変位(秒)"
                   | y == 031 = "次の値に関連する継続時間（時）"
                   | y == 032 = "次の値に関連する継続時間（分）"
                   | y == 041 = "時間差(UTCーLMT)（分）"
                   | y == 043 = "1月1日から数えた日"
                   | y == 051 = "日最高気温の主要読み取り時刻"
                   | y == 052 = "日最低気温の主要読み取り時刻"
                   | y == 053 = "日降水量1mm以上の日数"
                   | y == 059 = "通報された平均値を算出するために用いた観測時刻"
                   | y == 065 = "時間増分(短・分)"
                   | y == 066 = "Short Time Increment（秒）"
                   | y == 073 = "期間または時間変位(短・日)"
                   | y == 074 = "期間または時間変位(短・時)"
                   | y == 075 = "期間または時間変位(短・分)"
                   | y == 080 = "次の値の平均時間"
                   | y == 086 = "期間または時間変位(長・秒)"
                   | y == 192 = "解析・予報期間の識別符"
                   | y == 193 = "年(JST)"
                   | y == 194 = "月(JST)"
                   | y == 195 = "日(JST)"
                   | y == 196 = "時(JST)"
                   | y == 197 = "分(JST)"
                   | y == 198 = "解析・予報期間の識別符"

  show (FXY 0 5 y) | y == 001 = "緯度(高精度)"
                   | y == 002 = "緯度（低精度）"
                   | y == 011 = "緯度増分（高精度）"
                   | y == 012 = "緯度増分（低精度）"
                   | y == 015 = "緯度変位（高精度）"
                   | y == 016 = "緯度変位（低精度）"
                   | y == 021 = "方位角"
                   | y == 022 = "太陽の方位角"
                   | y == 023 = "太陽の衛星に対する方位角差"
                   | y == 030 = "方向（スペクトル）"
                   | y == 031 = "横列番号（row number）"
                   | y == 033 = "水平軸１の方向のピクセルサイズ"
                   | y == 034 = "アロングトラック横列番号"
                   | y == 035 = "Maximum size of x-dimension"
                   | y == 036 = "ＳＯＯＰで定めた船舶のトランセクト(transect)番号"
                   | y == 040 = "軌道番号"
                   | y == 041 = "走査線番号"
                   | y == 042 = "チャンネル番号"
                   | y == 043 = "視野角番号"
                   | y == 044 = "周期番号"
                   | y == 045 = "Field of regard number"
                   | y == 052 = "チャンネル番号の増分"
                   | y == 053 = "視野角番号の増分"
                   | y == 060 = "Y angular position from centre of gravity"
                   | y == 061 = "Z angular position from centre of gravity"
                   | y == 063 = "Spacecraft Roll"
                   | y == 064 = "Spacecraft Pitch"
                   | y == 066 = "Spacecraft Yaw"
                   | y == 067 = "Number of scan lines"
                   | y == 068 = "Profile number"
                   | y == 069 = "Receiver channel"
                   | y == 070 = "Observation identifier"
                   | y == 192 = "台風の位置（緯度）"
                   | y == 193 = "台風の位置（緯度）"
                   | y == 194 = "方位"
                   | y == 240 = "一次メッシュ緯度番号"
                   | y == 241 = "二次メッシュ緯度番号"
                   | y == 242 = "三次メッシュ緯度番号"
  show (FXY 0 6 y) | y == 001 = "緯度(高精度)"
                   | y == 002 = "経度（低精度）"
                   | y == 011 = "経度増分（高精度）"
                   | y == 012 = "経度増分（低精度）"
                   | y == 015 = "経度変位（高精度）"
                   | y == 016 = "経度変位（低精度）"
                   | y == 021 = "距離"
                   | y == 029 = "Wave number[m-1]"
                   | y == 030 = "波数（スペクトル） [rad/m]"
                   | y == 031 = "縦列番号（column number）"
                   | y == 033 = "水平軸２の方向のピクセルサイズ"
                   | y == 034 = "クロストラックセル番号"
                   | y == 035 = "Maximum size of y-dimension"
                   | y == 040 = "位置（特定信頼度の半径）"
                   | y == 192 = "台風の位置（経度）"
                   | y == 193 = "台風の位置（経度）"
                   | y == 194 = "距離"
                   | y == 241 = "一次メッシュ経度番号"
                   | y == 242 = "二次メッシュ経度番号"
                   | y == 243 = "三次メッシュ経度番号"
  show (FXY 0 7 y) | y == 001 = "観測所の標高[m]"
                   | y == 002 = "高さ又は海抜高度[m]"
                   | y == 003 = "ジオポテンシャル[m2/s2]"
                   | y == 004 = "気圧[Pa]"
                   | y == 005 = "高さの増分[m]"
                   | y == 006 = "観測所からの高さ[m]"
                   | y == 007 = "高さ[m]"
                   | y == 008 = "ジオポテンシャル[m2/s2]"
                   | y == 009 = "ジオポテンシャル高度[gpm]"
                   | y == 010 = "フライトレベル[m]"
                   | y == 012 = "格子点高度[m]"
                   | y == 021 = "高度角[deg]"
                   | y == 022 = "太陽の高度角[deg]"
                   | y == 024 = "衛星の天頂角[deg]"
                   | y == 025 = "太陽の天頂角[deg]"
                   | y == 026 = "衛星の天頂角[deg]"
                   | y == 030 = "平均海面からの観測所の標高[m]"
                   | y == 031 = "平均海面からの気圧計の高さ[m]"
                   | y == 032 = "地面（local ground）（又は海洋プラットフォームの甲板）からのセンサーの高さ[m]"
                   | y == 033 = "水面からのセンサーの高さ[m]"
                   | y == 035 = "Maximum size of z-dimension"
                   | y == 036 = "Level index of z"
                   | y == 040 = "影響パラメータ[m]"
                   | y == 061 = "地面からの深度[m]"
                   | y == 062 = "海面／水面からの深度[m]"
                   | y == 063 = "海面／水面からの深度（cm単位）[m]"
                   | y == 064 = "測器の観測所からの代表的高さ[m]"
                   | y == 065 = "水圧[Pa]"
                   | y == 070 = "ドローグの深さ[m]"
                   | y == 071 = "Height (high resolution)[m]"
                   | y == 200 = "検潮所の固定点（球分体）[m]"
                   | y == 201 = "検潮所の観測基準面の標高[m]"
  show (FXY 0 8 y) | y == 001 = "鉛直観測位置の名称"
                   | y == 002 = "鉛直位置の名称（地表観測）"

  show (FXY 0 11 y) | y == 001 = "風向"
  show (FXY 0 11 y) | y == 002 = "風速"
  show (FXY 0 11 y) | y == 003 = "u成分（東西成分）"
  show (FXY 0 11 y) | y == 004 = "v成分（南北成分）"
  show (FXY 0 11 y) | y == 005 = "w成分（鉛直成分：Pa/s）"
  show (FXY 0 11 y) | y == 006 = "w成分（東西成分：m/s）"

wmoCountry v | 47200 <= v && v <= 47998 = "Japan"
wmoCountry v | otherwise = "Not Japan"


bufrFlag u scale refV bits = BUFRFlag u scale refV bits

-- BUFRFlag = Flag Unit (Scale Int) (RefV Int) (InfoBit Int) deriving Eq, Show
bufrAtTableRef :: TableRefID -> BUFRFlag
bufrAtTableRef (FXY 0   2   1) = bufrFlag NA 0 0 2 
bufrAtTableRef (FXY 0   2   1) = bufrFlag NA 0 0 2 
bufrAtTableRef (FXY 0   2   1) = bufrFlag NA 0 0 2 
bufrAtTableRef (FXY 0   2   1) = bufrFlag NA 0 0 2 
bufrAtTableRef (FXY 0   2   1) = bufrFlag NA 0 0 2 
bufrAtTableRef (FXY 0   5   1) = bufrFlag DEG 5 (-9000000) 25 
bufrAtTableRef (FXY 0   5   2) = bufrFlag DEG 2 (-   9000) 15
bufrAtTableRef (FXY 0   5  11) = bufrFlag DEG 5 (-9000000) 25 
bufrAtTableRef (FXY 0   5  12) = bufrFlag DEG 2 (-   9000) 15
bufrAtTableRef (FXY 0   5  15) = bufrFlag DEG 5 (-9000000) 25
bufrAtTableRef (FXY 0   5  16) = bufrFlag DEG 2 (-   9000) 15
-- 気圧・高さ関連
bufrAtTableRef (FXY 0   7   6) = bufrFlag DEG 0 (-      0) 15
-- 風速関連
bufrAtTableRef (FXY 0  11   1) = bufrFlag DEG  0 (     0)  9 -- 風向
bufrAtTableRef (FXY 0  11   2) = bufrFlag DEG  1 (     0) 12 -- 風速
bufrAtTableRef (FXY 0  11   3) = bufrFlag M_S  1 (- 4096) 13 -- u成分（東西成分）
bufrAtTableRef (FXY 0  11   4) = bufrFlag M_S  1 (- 4096) 13 -- v成分（南北成分）
bufrAtTableRef (FXY 0  11   5) = bufrFlag PA_S 1 (-  512) 10 -- w成分[Pa/s]
bufrAtTableRef (FXY 0  11   6) = bufrFlag M_S  2 (- 4096) 13 -- w成分（鉛直成分）

bufrAtTableRef (FXY 0  21  30) = bufrFlag  DB 0 (-    32) 8 -- w成分（鉛直成分）
bufrAtTableRef _ = undefined 

bufrOfElem :: BR.BUFRElement -> TableRefID
bufrOfElem BR.HeightFromStation = FXY 0  7  6
bufrOfElem BR.WindDirection = FXY 0 11 1
bufrOfElem BR.WindSpeed     = FXY 0 11 2
bufrOfElem BR.WindWE        = FXY 0 11 3
bufrOfElem BR.WindSN        = FXY 0 11 4
bufrOfElem BR.WindV_PaS     = FXY 0 11 5
bufrOfElem BR.WindV_MS      = FXY 0 11 6
bufrOfElem BR.SN            = FXY 0 21 30

bufrOfElem _             = undefined

(.|) :: Float -> BR.BUFRElement -> Float
v .| be = let (BUFRFlag _ s r _) = bufrAtTableRef $  bufrOfElem be
          in (v+fromIntegral r)/10**fromIntegral s

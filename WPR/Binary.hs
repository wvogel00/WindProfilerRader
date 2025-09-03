{-# LANGUAGE OverloadedStrings #-}

module WPR.Binary where
-- 電信で得られたバイナリファイルからデータを取得する

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Binary.Get
import Data.Word (Word8)
import Data.Char (chr)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as BL
import WPR.Difinition
import qualified WPR.BUFR as BUFR
import           WPR.BUFR ((.|))
import qualified WPR.BUFRRef as BR
import Debug.Trace
import Data.Bits

samplePath = "data/IUPC41_RJTD_020000_202509020016141_001.send"

mapChr = map (chr . fromIntegral)
seqWord8 n = sequence $ replicate n getWord8

headerP :: Get (IUPC, String)
headerP = do
  iupcStr <- mapChr <$> seqWord8 4
  n <- read . mapChr <$> seqWord8 2
  getWord8
  rjtdStr <- mapChr <$> seqWord8 4
  getWord8
  d <- read . mapChr <$> seqWord8 2 :: Get Int -- utc
  h <- read . mapChr <$> seqWord8 2 :: Get Int -- utc
  m <- read . mapChr <$> seqWord8 2 :: Get Int -- utc
  corrFlag <- mapChr <$> lookAhead (seqWord8 4)
  let str = rjtdStr ++ "date=" ++ show [d,h,m]
  if " CC" `isInfixOf` corrFlag
    then seqWord8 4 >> return (IUPC n, str++corrFlag)
    else return (IUPC n, str) 

seqNums :: Int -> Get [Int]
seqNums n = map fromIntegral <$> seqWord8 n


seqNum :: Int -> Get Int
seqNum n = do
  xs <- map fromIntegral <$> seqWord8 n
  return $ sum . map f $ zip [0, 8 ..] (reverse xs) where
    f (k, v) = 2^k * v

block0P :: Get (String, Int, Int)
block0P = do
  bufrStr <- mapChr <$> seqWord8 4
  totalLen <- seqNum 3
  ver <- fromIntegral <$> getWord8
  return (bufrStr,totalLen, ver)

block1P :: Get Block1 
block1P = do
  b1Len <- seqNum 3
  masterTable <- seqNum 1
  madeBy <- seqNum 2
  madeBySub <- seqNum 2
  updateNo <- seqNum 1
  notForall <- seqNum 1
  vertRef <- seqNum 1 -- 衛星を除く == 2
  catUniverse <- seqNum 1
  catRegion <- seqNum 1
  masterVer <- seqNum 1 -- 現行 == 12
  localVer <- seqNum 1
  year <- seqNum 2
  month <- seqNum 1
  day <- seqNum 1
  hour <- seqNum 1
  minute <- seqNum 1
  second <- seqNum 1
  return $ Block1 { len =  b1Len
                  , masterTable = masterTable
                  , madeBy = (madeBy, madeBySub)
                  , updateNo = updateNo
                  , notForall = notForall
                  , vertRef = vertRef
                  , category = (catUniverse, catRegion)
                  , ver = (masterVer, localVer)
                  , madedate = Date year month day hour minute second
                  }

concatNum1000 xs = f 0 xs where
  f v [] = v
  f v (x:xs) = f (x + v*1000) xs 

block3P :: Get Block3
block3P = do
  len      <- seqNum 3
  keep     <- seqNum 1
  staN     <- seqNum 2
  compress <- seqNum 1
  wmoBID   <- seqNum 2
  wmoPID   <- seqNum 2
  lon      <- seqNum 2
  lat      <- seqNum 2
  --lat      <- concatNum1000 <$> seqNums 2
  antH      <-seqNum 2
  dev      <- seqNum 2
  lN1      <- seqNum 2
  rN1      <- seqNum 2
  [y,m,d,h,mt] <- sequence (replicate 5 $ seqNum 2)
  tID       <- seqNum 2
  tspan     <- seqNum 2
  lN2       <- seqNum 2
  rN2       <- seqNum 2
  measH     <- fromIntegral <$> seqNum 2
  localRefW <- seqNum 2
  quality   <- seqNum 2
  weWind    <- fromIntegral <$> seqNum 2
  nsWind    <- fromIntegral <$> seqNum 2
  vWind     <- fromIntegral <$> seqNum 2
  sn        <- fromIntegral <$> seqNum 2
  return $ Block3 { b3Len = len
                  , b3Keep = keep == 0
                  , stationN = staN
                  , compress = compress == 128
                  , wmoBlockID = wmoBID == 256+1
                  , wmoPointID = wmoPID == 256+2
                  , b3Pos = lat == 256*6+2 && lon==256*5+2
                  , antHeight = antH == 256*7+1
                  , device = dev == 256*2+3
                  , latencyN = lN1 == 0x5000, repeatN = rN1 == 0x1f01
                  , measDate = (y,m,d,h,mt) == (1025, 1026, 1027, 1028, 1029)
                  , timeID = tID == 256*8 + 21
                  , timeSpan = tspan == 256*4+25
                  , latencyN2 = lN2 == 0x4700, repeatN2 = rN2 == 0x1f01
                  , height = measH == 256*7+6
                  , localRefWidth = localRefW == 0x8608
                  , qualityInfo = quality == 256*25+192
                  , measWind = (weWind, nsWind, vWind) == (2819,2820,2822)
                  , sn = sn == 256*21+30
                  }
data BitState = BitState { buf :: Int, nbits :: Int} deriving (Eq, Show)

-- 読むビット数・バイナリファイルの読み取り状態を受け取り，読み値と読み取り状態を返す
getBits :: Int -> BitState -> Get (Int, BitState)
getBits n bs@(BitState b k)
  | n <= k = let val  = b `shiftR` (k - n)
                 mask = (1 `shiftL` n) - 1
                 rest = b .&. ((1 `shiftL` (k - n)) - 1)
             in return (val .&. mask, BitState rest (k - n))
  | otherwise = do
      w <- getWord8                      -- 1バイト読む
      let b' = (b `shiftL` 8) .|. fromIntegral w
          k' = k + 8
      getBits n (BitState b' k')

--block4P :: Get Block4
block4P wmoDB = do
  l     <- seqNum 3
  keep  <- seqNum 1
  [wmoBID, wmoPID, lon]  <- separate1 <$> seqNum 4
  lat   <- seqNum 2
  (antH, bs1)  <- getBits 15 $ BitState 0 0
  (device, bs2) <- getBits 4 bs1
  (rN, bs3) <- getBits 8 bs2
  (ms, bs4) <- measurementsP rN bs3
  return Block4 { b4Len = l
                , b4Keep = keep
                , _wmoBlockID = wmoBID, _wmoPointID = wmoPID
                , _areaName = BUFR.wmoPointName wmoBID wmoPID wmoDB
                , _antHeight = fromIntegral antH
                , _pos = (fromIntegral lat / 100, fromIntegral lon / 100)
                , _device = BUFR.deviceName device
                , _repeatN = rN
                , measurement = ms
                }
  where
    separate1 v = [shift v (-25), 0x03ff .&. shift v (-15), 0x7fff .&. v] 

measurementsP :: Int -> BitState -> Get ([Measurement], BitState)
measurementsP 0 bs = return ([], bs)
measurementsP i bs = do
  ( y, bs1)  <-trace (show i ++" year") <$> getBits 12 bs
  ( m, bs2)  <- getBits  4 bs1
  ( d, bs3)  <- getBits  6 bs2
  ( h, bs4)  <- getBits  5 bs3
  (mt, bs5)  <- getBits  6 bs4
  (tID, bs6) <- getBits  5 bs5
  (ts, bs7)  <- getBits 12 bs6
  (rN, bs8)  <- getBits  8 bs7
  (ws, bs9)  <- windsP rN bs8
  (ms, bs10)  <- measurementsP (i-1) bs9
  return $ ( Measurement { mDate   = Date y m d h mt 0
                         , mTimeID = tID
                         , mSpan   = ts
                         , mRepeatN = rN
                         , measuredWinds = ws
                         } : ms , bs10)

windsP :: Int -> BitState -> Get ([MeasuredWind], BitState)
windsP 0 bs = return ([], bs)
windsP i bs = do
  (  mwH, bs1) <- convert BR.HeightFromStation <$> getBits 15 bs
  (    q, bs2) <- getBits  8 bs1
  (wWind, bs3) <- convert BR.WindWE <$> getBits 13 bs2
  (sWind, bs4) <- convert BR.WindSN <$> getBits 13 bs3
  (vWind, bs5) <- convert BR.WindV_MS <$> getBits 13 bs4
  (   sn, bs6) <- convert BR.SN <$> getBits  8 bs5
  (   ws, bs7) <- windsP (i-1) bs6 
  let wind = Wind (S $ sWind) (W $ wWind) (V $ vWind) 
  let mValue = MeasuredWind mwH wind sn
  return $ ( mValue:ws, bs7)
  where
    convert :: BR.BUFRElement -> (Int, BitState) -> (Float, BitState)
    convert brElem (v,bs) = (fromIntegral v .| brElem, bs)





--wprP :: Show a => Get a
wprP wmoTable = do
  header <- headerP
  block0 <- block0P
  block1 <- block1P
  block3 <- block3P
  block4 <- block4P wmoTable
  return (header, block0, block1, block3, block4)

analyze :: FilePath -> IO()
analyze file = do
  wmoTable <- BUFR.readWMOTable
  bs <- BL.readFile file
  let wrp = runGet (wprP wmoTable) bs
  print wrp


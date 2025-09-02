module WPR.Binary where
-- 電信で得られたバイナリファイルからデータを取得する

import Data.Binary.Get
import Data.Word (Word8)
import Data.Char (chr)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as BL
import WPR.Difinition

 --data WPR = WPR { area   :: IUPC
 --                , date   :: Date
 --                , height :: Float
 --                , pos    :: (Float, Float)
 --                , meas   :: [ (Date, [Wind, SN] ) ]
 --                } deriving Eq

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

--wprP :: Show a => Get a
wprP = do
  header <- headerP
  block0 <- block0P
  block1 <- block1P
  return (header, block0, block1)

analyze :: FilePath -> IO()
analyze file = do
  bs <- BL.readFile file
  let wrp = runGet wprP bs
  print wrp


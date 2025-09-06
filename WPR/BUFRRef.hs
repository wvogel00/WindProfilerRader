module WPR.BUFRRef where

data BUFRElement = WindDirection | WindSpeed | WindWE | WindSN | WindV_PaS | WindV_MS -- 02
                 | Year | Month | Day | Hour | Minute | Second | Second_US | DeltaYear | DeltaMonth | DeltaDay | DeltaHour | DeltaMinute | DeltaSecond | YearSpan | MonthSpan | DaySpan | HourSpan | MinuteSpan | SecondSpan -- 04
                 | StationHeight | Height | GeoPotential | Pa | DeltaHeight | HeightFromStation -- 07
                 | TimeIdentify-- 08
                 | SN -- 21
                 | DelayFlag | DelayRepeat | DelayRepeatEX | DelayBitRepeat | DelayBitRepeatEX | DelayBind | DelayBitExist -- 31
                 deriving (Eq, Show)

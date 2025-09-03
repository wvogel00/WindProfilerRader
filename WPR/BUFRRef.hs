module WPR.BUFRRef where

data BUFRElement = WindDirection | WindSpeed | WindWE | WindSN | WindV_PaS | WindV_MS -- 02
                 | StationHeight | Height | GeoPotential | Pa | DeltaHeight | HeightFromStation -- 07
                 | SN -- 21 
                 deriving (Eq, Show)

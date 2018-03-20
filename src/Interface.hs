module Interface where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Types


loadImages :: IO Images
loadImages = do
    Just idle1              <- loadJuicyPNG "png/Idle (1).png"
    Just idle2              <- loadJuicyPNG "png/Idle (2).png"
    Just idle3              <- loadJuicyPNG "png/Idle (3).png"
    Just idle4              <- loadJuicyPNG "png/Idle (4).png"
    Just idle5              <- loadJuicyPNG "png/Idle (5).png"

    return Images {
      image1 = scale 0.07 0.07 idle1,
      image2 = scale 0.07 0.07 idle2,
      image3 = scale 0.07 0.07 idle3,
      image4 = scale 0.07 0.07 idle4,
      image5 = scale 0.07 0.07 idle5
    }



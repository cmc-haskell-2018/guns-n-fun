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

    Just run1              <- loadJuicyPNG "png/Run (1).png"
    Just run2              <- loadJuicyPNG "png/Run (2).png"
    Just run3              <- loadJuicyPNG "png/Run (3).png"
    Just run4              <- loadJuicyPNG "png/Run (4).png"
    Just run5              <- loadJuicyPNG "png/Run (5).png"
    Just run6              <- loadJuicyPNG "png/Run (6).png"
    Just run7              <- loadJuicyPNG "png/Run (7).png"
    Just run8              <- loadJuicyPNG "png/Run (8).png"



    return Images {
      image1 = scale 0.07 0.07 idle1,
      image2 = scale 0.07 0.07 idle2,
      image3 = scale 0.07 0.07 idle3,
      image4 = scale 0.07 0.07 idle4,
      image5 = scale 0.07 0.07 idle5,

      image11 = scale 0.07 0.07 run1,
      image12 = scale 0.07 0.07 run2,
      image13 = scale 0.07 0.07 run3,
      image14 = scale 0.07 0.07 run4,
      image15 = scale 0.07 0.07 run5,
      image16 = scale 0.07 0.07 run6,
      image17 = scale 0.07 0.07 run7,
      image18 = scale 0.07 0.07 run8

    }



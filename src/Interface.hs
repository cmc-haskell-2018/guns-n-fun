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
    Just idle6              <- loadJuicyPNG "png/Idle (6).png"
    Just idle7              <- loadJuicyPNG "png/Idle (7).png"
    Just idle8              <- loadJuicyPNG "png/Idle (8).png"
    Just idle9              <- loadJuicyPNG "png/Idle (9).png"
    Just idle10             <- loadJuicyPNG "png/Idle (10).png"

    Just dead1              <- loadJuicyPNG "png/Dead (1).png"

    Just jump1              <- loadJuicyPNG "png/Jump (1).png"

    Just run1               <- loadJuicyPNG "png/Run (1).png"
    Just run2               <- loadJuicyPNG "png/Run (2).png"
    Just run3               <- loadJuicyPNG "png/Run (3).png"
    Just run4               <- loadJuicyPNG "png/Run (4).png"
    Just run5               <- loadJuicyPNG "png/Run (5).png"
    Just run6               <- loadJuicyPNG "png/Run (6).png"
    Just run7               <- loadJuicyPNG "png/Run (7).png"
    Just run8               <- loadJuicyPNG "png/Run (8).png"


    return Images {
      image1  = scale 0.2 0.2 idle1,
      image2  = scale 0.2 0.2 idle2,
      image3  = scale 0.2 0.2 idle3,
      image4  = scale 0.2 0.2 idle4,
      image5  = scale 0.2 0.2 idle5,
      image6  = scale 0.2 0.2 idle6,
      image7  = scale 0.2 0.2 idle7,
      image8  = scale 0.2 0.2 idle8,
      image9  = scale 0.2 0.2 idle9,
      image10 = scale 0.2 0.2 idle10,
      image21 = scale 0.2 0.2 dead1,
      image31 = scale 0.2 0.2 jump1,
      image41  = scale 0.2 0.2 run1,
      image42  = scale 0.2 0.2 run2,
      image43  = scale 0.2 0.2 run3,
      image44  = scale 0.2 0.2 run4,
      image45  = scale 0.2 0.2 run5,
      image46  = scale 0.2 0.2 run6,
      image47  = scale 0.2 0.2 run7,
      image48  = scale 0.2 0.2 run8
    }



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

    Just jump1             <- loadJuicyPNG "png/Jump (1).png"
    Just jump2             <- loadJuicyPNG "png/Jump (2).png"
    Just jump3             <- loadJuicyPNG "png/Jump (3).png"
    Just jump4             <- loadJuicyPNG "png/Jump (4).png"
    Just jump5             <- loadJuicyPNG "png/Jump (5).png"
    Just jump6             <- loadJuicyPNG "png/Jump (6).png"
    Just jump7             <- loadJuicyPNG "png/Jump (7).png"
    Just jump8             <- loadJuicyPNG "png/Jump (8).png"
    Just jump9             <- loadJuicyPNG "png/Jump (9).png"
    Just jump10            <- loadJuicyPNG "png/Jump (10).png"

    Just jump1r             <- loadJuicyPNG "png/Jump (1)R.png"
    Just jump2r             <- loadJuicyPNG "png/Jump (2)R.png"
    Just jump3r             <- loadJuicyPNG "png/Jump (3)R.png"
    Just jump4r             <- loadJuicyPNG "png/Jump (4)R.png"
    Just jump5r             <- loadJuicyPNG "png/Jump (5)R.png"
    Just jump6r             <- loadJuicyPNG "png/Jump (6)R.png"
    Just jump7r             <- loadJuicyPNG "png/Jump (7)R.png"
    Just jump8r             <- loadJuicyPNG "png/Jump (8)R.png"
    Just jump9r             <- loadJuicyPNG "png/Jump (9)R.png"
    Just jump10r            <- loadJuicyPNG "png/Jump (10)R.png"





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
      image18 = scale 0.07 0.07 run8,

      image21 = scale 0.07 0.07 jump1,
      image22 = scale 0.07 0.07 jump2,
      image23 = scale 0.07 0.07 jump3,
      image24 = scale 0.07 0.07 jump4,
      image25 = scale 0.07 0.07 jump5,
      image26 = scale 0.07 0.07 jump6,
      image27 = scale 0.07 0.07 jump7,
      image28 = scale 0.07 0.07 jump8,
      image29 = scale 0.07 0.07 jump9,
      image30 = scale 0.07 0.07 jump10,

      image31 = scale 0.07 0.07 jump1r,
      image32 = scale 0.07 0.07 jump2r,
      image33 = scale 0.07 0.07 jump3r,
      image34 = scale 0.07 0.07 jump4r,
      image35 = scale 0.07 0.07 jump5r,
      image36 = scale 0.07 0.07 jump6r,
      image37 = scale 0.07 0.07 jump7r,
      image38 = scale 0.07 0.07 jump8r,
      image39 = scale 0.07 0.07 jump9r,
      image40 = scale 0.07 0.07 jump10r


    }



module Interface where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Types


loadImages :: IO Images
loadImages = do
    Just p1idle0             <- loadJuicyPNG "png/Player1/Idle (1)R.png"
    Just p1idle1             <- loadJuicyPNG "png/Player1/Idle (1).png"
    Just p1idle2             <- loadJuicyPNG "png/Player1/Idle (2).png"
    Just p1idle3             <- loadJuicyPNG "png/Player1/Idle (3).png"
    Just p1idle4             <- loadJuicyPNG "png/Player1/Idle (4).png"
    Just p1idle5             <- loadJuicyPNG "png/Player1/Idle (5).png"

    Just p1run1              <- loadJuicyPNG "png/Player1/Run (1).png"
    Just p1run2              <- loadJuicyPNG "png/Player1/Run (2).png"
    Just p1run3              <- loadJuicyPNG "png/Player1/Run (3).png"
    Just p1run4              <- loadJuicyPNG "png/Player1/Run (4).png"
    Just p1run5              <- loadJuicyPNG "png/Player1/Run (5).png"
    Just p1run6              <- loadJuicyPNG "png/Player1/Run (6).png"
    Just p1run7              <- loadJuicyPNG "png/Player1/Run (7).png"
    Just p1run8              <- loadJuicyPNG "png/Player1/Run (8).png"

    Just p1jump1             <- loadJuicyPNG "png/Player1/Jump (1).png"
    Just p1jump2             <- loadJuicyPNG "png/Player1/Jump (2).png"
    Just p1jump3             <- loadJuicyPNG "png/Player1/Jump (3).png"
    Just p1jump4             <- loadJuicyPNG "png/Player1/Jump (4).png"
    Just p1jump5             <- loadJuicyPNG "png/Player1/Jump (5).png"
    Just p1jump6             <- loadJuicyPNG "png/Player1/Jump (6).png"
    Just p1jump7             <- loadJuicyPNG "png/Player1/Jump (7).png"
    Just p1jump8             <- loadJuicyPNG "png/Player1/Jump (8).png"
    Just p1jump9             <- loadJuicyPNG "png/Player1/Jump (9).png"
    Just p1jump10            <- loadJuicyPNG "png/Player1/Jump (10).png"

    Just p1jump1r             <- loadJuicyPNG "png/Player1/Jump (1)R.png"
    Just p1jump2r             <- loadJuicyPNG "png/Player1/Jump (2)R.png"
    Just p1jump3r             <- loadJuicyPNG "png/Player1/Jump (3)R.png"
    Just p1jump4r             <- loadJuicyPNG "png/Player1/Jump (4)R.png"
    Just p1jump5r             <- loadJuicyPNG "png/Player1/Jump (5)R.png"
    Just p1jump6r             <- loadJuicyPNG "png/Player1/Jump (6)R.png"
    Just p1jump7r             <- loadJuicyPNG "png/Player1/Jump (7)R.png"
    Just p1jump8r             <- loadJuicyPNG "png/Player1/Jump (8)R.png"
    Just p1jump9r             <- loadJuicyPNG "png/Player1/Jump (9)R.png"
    Just p1jump10r            <- loadJuicyPNG "png/Player1/Jump (10)R.png"


    Just p1run1r              <- loadJuicyPNG "png/Player1/Run (1)R.png"
    Just p1run2r              <- loadJuicyPNG "png/Player1/Run (2)R.png"
    Just p1run3r              <- loadJuicyPNG "png/Player1/Run (3)R.png"
    Just p1run4r              <- loadJuicyPNG "png/Player1/Run (4)R.png"
    Just p1run5r              <- loadJuicyPNG "png/Player1/Run (5)R.png"
    Just p1run6r              <- loadJuicyPNG "png/Player1/Run (6)R.png"
    Just p1run7r              <- loadJuicyPNG "png/Player1/Run (7)R.png"
    Just p1run8r              <- loadJuicyPNG "png/Player1/Run (8)R.png"


    Just p2idle0              <- loadJuicyPNG "png/Player2/Idle (1)R.png"
    Just p2idle1              <- loadJuicyPNG "png/Player2/Idle (1).png"
    Just p2idle2              <- loadJuicyPNG "png/Player2/Idle (2).png"
    Just p2idle3              <- loadJuicyPNG "png/Player2/Idle (3).png"
    Just p2idle4              <- loadJuicyPNG "png/Player2/Idle (4).png"
    Just p2idle5              <- loadJuicyPNG "png/Player2/Idle (5).png"

    Just p2run1               <- loadJuicyPNG "png/Player2/Run (1).png"
    Just p2run2               <- loadJuicyPNG "png/Player2/Run (2).png"
    Just p2run3               <- loadJuicyPNG "png/Player2/Run (3).png"
    Just p2run4               <- loadJuicyPNG "png/Player2/Run (4).png"
    Just p2run5               <- loadJuicyPNG "png/Player2/Run (5).png"
    Just p2run6               <- loadJuicyPNG "png/Player2/Run (6).png"
    Just p2run7               <- loadJuicyPNG "png/Player2/Run (7).png"
    Just p2run8               <- loadJuicyPNG "png/Player2/Run (8).png"

    Just p2jump1              <- loadJuicyPNG "png/Player2/Jump (1).png"
    Just p2jump2              <- loadJuicyPNG "png/Player2/Jump (2).png"
    Just p2jump3              <- loadJuicyPNG "png/Player2/Jump (3).png"
    Just p2jump4              <- loadJuicyPNG "png/Player2/Jump (4).png"
    Just p2jump5              <- loadJuicyPNG "png/Player2/Jump (5).png"
    Just p2jump6              <- loadJuicyPNG "png/Player2/Jump (6).png"
    Just p2jump7              <- loadJuicyPNG "png/Player2/Jump (7).png"
    Just p2jump8              <- loadJuicyPNG "png/Player2/Jump (8).png"
    Just p2jump9              <- loadJuicyPNG "png/Player2/Jump (9).png"
    Just p2jump10             <- loadJuicyPNG "png/Player2/Jump (10).png"

    Just p2jump1r             <- loadJuicyPNG "png/Player2/Jump (1)R.png"
    Just p2jump2r             <- loadJuicyPNG "png/Player2/Jump (2)R.png"
    Just p2jump3r             <- loadJuicyPNG "png/Player2/Jump (3)R.png"
    Just p2jump4r             <- loadJuicyPNG "png/Player2/Jump (4)R.png"
    Just p2jump5r             <- loadJuicyPNG "png/Player2/Jump (5)R.png"
    Just p2jump6r             <- loadJuicyPNG "png/Player2/Jump (6)R.png"
    Just p2jump7r             <- loadJuicyPNG "png/Player2/Jump (7)R.png"
    Just p2jump8r             <- loadJuicyPNG "png/Player2/Jump (8)R.png"
    Just p2jump9r             <- loadJuicyPNG "png/Player2/Jump (9)R.png"
    Just p2jump10r            <- loadJuicyPNG "png/Player2/Jump (10)R.png"


    Just p2run1r              <- loadJuicyPNG "png/Player2/Run (1)R.png"
    Just p2run2r              <- loadJuicyPNG "png/Player2/Run (2)R.png"
    Just p2run3r              <- loadJuicyPNG "png/Player2/Run (3)R.png"
    Just p2run4r              <- loadJuicyPNG "png/Player2/Run (4)R.png"
    Just p2run5r              <- loadJuicyPNG "png/Player2/Run (5)R.png"
    Just p2run6r              <- loadJuicyPNG "png/Player2/Run (6)R.png"
    Just p2run7r              <- loadJuicyPNG "png/Player2/Run (7)R.png"
    Just p2run8r              <- loadJuicyPNG "png/Player2/Run (8)R.png"



    return Images {
      image0 = scale 0.07 0.07 p1idle0,
      image1 = scale 0.07 0.07 p1idle1,
      image2 = scale 0.07 0.07 p1idle2,
      image3 = scale 0.07 0.07 p1idle3,
      image4 = scale 0.07 0.07 p1idle4,
      image5 = scale 0.07 0.07 p1idle5,

      image11 = scale 0.07 0.07 p1run1,
      image12 = scale 0.07 0.07 p1run2,
      image13 = scale 0.07 0.07 p1run3,
      image14 = scale 0.07 0.07 p1run4,
      image15 = scale 0.07 0.07 p1run5,
      image16 = scale 0.07 0.07 p1run6,
      image17 = scale 0.07 0.07 p1run7,
      image18 = scale 0.07 0.07 p1run8,

      image21 = scale 0.07 0.07 p1jump1,
      image22 = scale 0.07 0.07 p1jump2,
      image23 = scale 0.07 0.07 p1jump3,
      image24 = scale 0.07 0.07 p1jump4,
      image25 = scale 0.07 0.07 p1jump5,
      image26 = scale 0.07 0.07 p1jump6,
      image27 = scale 0.07 0.07 p1jump7,
      image28 = scale 0.07 0.07 p1jump8,
      image29 = scale 0.07 0.07 p1jump9,
      image30 = scale 0.07 0.07 p1jump10,

      image31 = scale 0.07 0.07 p1jump1r,
      image32 = scale 0.07 0.07 p1jump2r,
      image33 = scale 0.07 0.07 p1jump3r,
      image34 = scale 0.07 0.07 p1jump4r,
      image35 = scale 0.07 0.07 p1jump5r,
      image36 = scale 0.07 0.07 p1jump6r,
      image37 = scale 0.07 0.07 p1jump7r,
      image38 = scale 0.07 0.07 p1jump8r,
      image39 = scale 0.07 0.07 p1jump9r,
      image40 = scale 0.07 0.07 p1jump10r,

      image41 = scale 0.07 0.07 p1run1r,
      image42 = scale 0.07 0.07 p1run2r,
      image43 = scale 0.07 0.07 p1run3r,
      image44 = scale 0.07 0.07 p1run4r,
      image45 = scale 0.07 0.07 p1run5r,
      image46 = scale 0.07 0.07 p1run6r,
      image47 = scale 0.07 0.07 p1run7r,
      image48 = scale 0.07 0.07 p1run8r,



      p2image0 = scale 0.07 0.07 p2idle0,
      p2image1 = scale 0.07 0.07 p2idle1,
      p2image2 = scale 0.07 0.07 p2idle2,
      p2image3 = scale 0.07 0.07 p2idle3,
      p2image4 = scale 0.07 0.07 p2idle4,
      p2image5 = scale 0.07 0.07 p2idle5,

      p2image11 = scale 0.07 0.07 p2run1,
      p2image12 = scale 0.07 0.07 p2run2,
      p2image13 = scale 0.07 0.07 p2run3,
      p2image14 = scale 0.07 0.07 p2run4,
      p2image15 = scale 0.07 0.07 p2run5,
      p2image16 = scale 0.07 0.07 p2run6,
      p2image17 = scale 0.07 0.07 p2run7,
      p2image18 = scale 0.07 0.07 p2run8,

      p2image21 = scale 0.07 0.07 p2jump1,
      p2image22 = scale 0.07 0.07 p2jump2,
      p2image23 = scale 0.07 0.07 p2jump3,
      p2image24 = scale 0.07 0.07 p2jump4,
      p2image25 = scale 0.07 0.07 p2jump5,
      p2image26 = scale 0.07 0.07 p2jump6,
      p2image27 = scale 0.07 0.07 p2jump7,
      p2image28 = scale 0.07 0.07 p2jump8,
      p2image29 = scale 0.07 0.07 p2jump9,
      p2image30 = scale 0.07 0.07 p2jump10,

      p2image31 = scale 0.07 0.07 p2jump1r,
      p2image32 = scale 0.07 0.07 p2jump2r,
      p2image33 = scale 0.07 0.07 p2jump3r,
      p2image34 = scale 0.07 0.07 p2jump4r,
      p2image35 = scale 0.07 0.07 p2jump5r,
      p2image36 = scale 0.07 0.07 p2jump6r,
      p2image37 = scale 0.07 0.07 p2jump7r,
      p2image38 = scale 0.07 0.07 p2jump8r,
      p2image39 = scale 0.07 0.07 p2jump9r,
      p2image40 = scale 0.07 0.07 p2jump10r,

      p2image41 = scale 0.07 0.07 p2run1r,
      p2image42 = scale 0.07 0.07 p2run2r,
      p2image43 = scale 0.07 0.07 p2run3r,
      p2image44 = scale 0.07 0.07 p2run4r,
      p2image45 = scale 0.07 0.07 p2run5r,
      p2image46 = scale 0.07 0.07 p2run6r,
      p2image47 = scale 0.07 0.07 p2run7r,
      p2image48 = scale 0.07 0.07 p2run8r

    }


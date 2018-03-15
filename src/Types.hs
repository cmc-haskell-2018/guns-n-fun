module Types where

import Graphics.Gloss
-- import Graphics.Gloss.Juicy
-- import Graphics.Gloss.Interface.Pure.Game

type Position = (Float, Float)
type SpeedVec = (Float, Float)


data GameState = Game
    {
    --   pLoc :: (Float, Float)
    -- , pSpeed :: (Float, Float)
        player1 :: Player
        -- player2 :: Player
        -- gameScore :: Float
        -- , bullets1 :: Bullet
        -- bullets2 :: [Bullet]

    } deriving Show


data Player = Player
  {
    pLoc :: (Float, Float)
    , pSpeedx :: Float
    , pSpeedy :: Float
    -- , isOnTheGeound :: Bool

  } deriving Show
data Bullet = Bullet
  {
    buLoc :: (Float, Float)
    , blocX :: Float
    , blocY :: Float
    , bSpeedx :: Float
    , bSpeedy :: Float

  } deriving Show

-- -- may be it'll be better not to move next several
-- -- lines to this file, but leave them there, 
-- -- where they'res used
-- width, height, offset :: Int
-- width = 300
-- height = 300
-- offset = 100


-- speed :: Float
-- speed = 20
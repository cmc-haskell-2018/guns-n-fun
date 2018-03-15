module Initialize where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Types

initLoc :: (Float, Float)
initLoc = (0,0)


-- initialBullets :: Player -> Bullet
-- initialBullets player1 = Bullet
--   {
--     buLoc  = (pLoc player1)
--     , blocX = (fst buLoc)
--     ,blocY = (snd buLoc)
--     , bSpeedx = 0
--     , bSpeedy = 0
--   }

initialPlayer :: Player
initialPlayer = Player
  {
        pLoc = initLoc
        , pSpeedx = 1
        , pSpeedy = 0

  }
makePlayer :: Position -> SpeedVec ->  Player
makePlayer (x,y) (x', y') = Player {
  pLoc = (x,y)
  , pSpeedx = x'
  , pSpeedy = y'
}

initialState :: GameState
initialState = Game
    {
         player1 = initialPlayer
         -- , bullets1 = initialBullets player1
    }
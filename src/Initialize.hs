module Initialize where

import Prelude
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Types as T

initLoc :: (Float, Float)
initLoc = (0,0)


initialBullets :: Player -> Bullet
initialBullets player1 = Bullet
  {
    -- buLoc  = (pLoc player1)
     blocX = (fst (pLoc player1))
    ,blocY = (snd (pLoc player1))
    , bSpeedx = (pSpeedx player1) + (if (pSpeedx player1) >= 0 then 2 else (-2))
    , bSpeedy = (pSpeedy player1)

  }


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
         , bullets1 =   T.Nothing 
         -- , bullets1 =   initialBullets initialPlayer
    }
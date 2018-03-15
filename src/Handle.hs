module Handle where

import Prelude
import Graphics.Gloss
import Graphics.Gloss.Juicy
-- import Graphics.Gloss.Internals.Interface.Game    
-- import Graphics.Gloss.Internals.Interface.Event    
import Graphics.Gloss.Interface.Pure.Game    
import Types
import Initialize
import Update


speed :: Float
speed = 20

-- -- | Respond to key events.
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'a') Down _ _ ) game = movePlayerLeft game
handleKeys (EventKey (Char 'a') Up _ _ ) game   = stopPlayer game
handleKeys (EventKey (Char 'd') Down _ _ ) game           = movePlayerRight game
handleKeys (EventKey (Char 'd') Up _ _ ) game             = stopPlayer game
-- handleKeys (EventKey (Char 'w') Down _ _ ) game = movePlayerLeft game
-- handleKeys (EventKey (Char 'w') Up _ _ ) game   = stopPlayer game
-- handleKeys (EventKey (Char 's') Down _ _ ) game           = movePlayerRight game
-- handleKeys (EventKey (Char 's') Up _ _ ) game             = stopPlayer game
handleKeys (EventKey (Char 'x') _ _ _ ) game              = initialState
handleKeys _ game                                         =  game





movePlayerLeft :: GameState -> GameState
movePlayerLeft game = game
  {
    player1 = move (player1 game)

  }
  where 
      move player = player
        {

          pSpeedx = ( -10)
        }


movePlayerRight :: GameState -> GameState
movePlayerRight game = game
  {
    player1 = move (player1 game)

  }
  where 
      move player = player {
      pSpeedx = 10}

-- stopPlayer :: GameState -> GameState
-- stopPlayer game = game
--   {
--     player1 = stop(player1  game)

--   }
--   where 
--     stop player = player{

--   }

stopPlayer :: GameState -> GameState
stopPlayer game = game
  {
      player1 = stop (player1 game )
  }
    where 
        stop player = player {
        pSpeedx = 0,
        pSpeedy = 0}

-- -- | Остановить игрока.
-- stopPlayer :: Universe -> Universe
-- stopPlayer u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerSpeed = 0}


-- bumpPlayerLeft :: Universe -> Universe
-- bumpPlayerLeft u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerSpeed = -bumpSpeed }

-- -- | Сдвинуть игрока вверх.
-- bumpPlayerUp :: Universe -> Universe
-- bumpPlayerUp u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerFallingSpeed = jumpSpeed }

-- -- |Сдвинуть игрока вправо.
-- bumpPlayerRight :: Universe -> Universe
-- bumpPlayerRight u = u
--   { universePlayer = bump (universePlayer u)
--   }
--   where
--     bump player = player {
--     playerSpeed = bumpSpeed }

module GunsAndFun where

import Prelude
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Types
import Initialize
import Render
import Handle
import Update

run :: IO ()
run = main2 --putStrLn "This project is not yet implemented"



width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = white
fps :: Int
fps = 60

main2 :: IO ()
main2 = play window background fps initialState render handleKeys update



-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, testquare]
  where
    --  The pong ball.
    ball = uncurry translate (pLoc (player1 game)) $ color ballColor $ circleSolid 10
    ballColor = dark red
    testquare = translate 10 20 $ color (light blue) $ rectangleSolid 20 30
    -- bullets1 = translate (blocX game)(blocY game) $ color red $ circleSolid 3




-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update  seconds =  movePlayer seconds


movePlayer :: Float -> GameState -> GameState
movePlayer seconds game = game  { player1  = move (player1 game) }
    where
        move player = player {
        pLoc = (x', y')
      }
        (x, y) = pLoc (player1  game)
        -- (vx, vy) = pSpeed (player1  game)
        vx = pSpeedx (player1 game)
        vy = pSpeedy (player1 game)
        x' = x + vx * seconds * 10
        y' = y + vy * seconds * 10



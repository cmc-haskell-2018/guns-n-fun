module GunsAndFun where

import Prelude
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Types as T
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
  pictures [ball, testquare, bull]
  where
    --  The pong ball.
    ball = uncurry translate (pLoc (player1 game)) $ color ballColor $ circleSolid 10
    ballColor = dark red
    testquare = translate 10 20 $ color (light blue) $ rectangleSolid 20 30
    bull = renderBullets (bullets1 game)
    -- bullets1 = translate (blocX game)(blocY game) $ color red $ circleSolid 3


renderBullets :: Bullet -> Picture
renderBullets (T.Nothing) = Blank --pictures [translate (-10) 10 $ color (light green) $ rectangleSolid 10 20]
renderBullets bullets = pictures [translate (blocX bullets) (blocY bullets) $ color (dark green) $ rectangleSolid 7 7]

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update  seconds =  movePlayerAndBullets seconds




movePlayerAndBullets :: Float -> GameState -> GameState
movePlayerAndBullets seconds game = game  { bullets1  = move (bullets1 game), player1  = moveP (player1 game) }
    where
        move bullet =case  bullet of Bullet{} -> Bullet{
        blocX = x'
        , blocY = y'
        , bSpeedx = vx
        , bSpeedy = vy
      }; (T.Nothing) -> T.Nothing
        moveP player = player {
        pLoc = (xP', yP')
      }
        x = (blocX (bullets1 game))
        y = (blocY (bullets1 game))
        (xP, yP) = pLoc (player1  game)
        -- (vx, vy) = pSpeed (player1  game)
        vx = bSpeedx (bullets1 game)
        vy = bSpeedy (bullets1 game)
        x' = x + vx * seconds * 10
        y' = y + vy * seconds * 10
        vxP = pSpeedx (player1 game)
        vyP = pSpeedy (player1 game)
        xP' = xP + vxP * seconds * 10
        yP' = yP + vyP * seconds * 10






















-- movePlayer :: Float -> GameState -> GameState
-- movePlayer seconds game = game  { player1  = move (player1 game) }
--     where
--         move player = player {
--         pLoc = (x', y')
--       }
--         (x, y) = pLoc (player1  game)
--         -- (vx, vy) = pSpeed (player1  game)
--         vx = pSpeedx (player1 game)
--         vy = pSpeedy (player1 game)
--         x' = x + vx * seconds * 10
--         y' = y + vy * seconds * 10

-- moveBullets :: Float -> GameState -> GameState
-- moveBullets seconds game = game  { bullets1  = move (bullets1 game) }
--     where
--         move bullet = bullet {
--         blocX = x'
--         , blocY = y'
--       }
--         x = (blocX (bullets1 game))
--         y = (blocY (bullets1 game))
--         -- (x, y) = pLoc (player1  game)
--         -- (vx, vy) = pSpeed (player1  game)
--         vx = bSpeedx (bullets1 game)
--         vy = bSpeedy (bullets1 game)
--         x' = x + vx * seconds * 10
--         y' = y + vy * seconds * 10

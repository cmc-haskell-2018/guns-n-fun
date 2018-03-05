module Main(main) where

import Prelude as PPP
import Graphics.Gloss
import  Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black


  where
    --  The pong ball.
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    -- --  The bottom and top walls.
    -- wall :: Float -> Picture
    -- wall offset =
    --   translate 0 offset $
    --     color wallColor $
    --       rectangleSolid 270 10

    -- wallColor = greyN 0.5
    -- walls = pictures [wall 150, wall (-150)]

    -- --  Make a paddle of a given border and vertical offset.
    -- mkPaddle :: Color -> Float -> Float -> Picture
    -- mkPaddle col x y = pictures
    --   [ translate x y $ color col $ rectangleSolid 26 86
    --   , translate x y $ color paddleColor $ rectangleSolid 20 80
    --   ]

    -- paddleColor = light (light blue)



-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  } deriving Show 






-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (10, 12-12)
  , player1 = 0
  , player2 = 10
  }



-- | Data describing the state of the pong game. 
--data PongGame = Game
--  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
--  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
--  , player1 :: Float           -- ^ Left player paddle height.
--                               -- Zero is the middle of the screen. 
--  , player2 :: Float           -- ^ Right player paddle height.
--  } deriving Show 



-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle (light green) 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid  300 10

    --  The left and right walls.
    vertWalls :: Float -> Picture
    vertWalls offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid 10 300

    wallColor = greyN 0.5
    walls = pictures [wall width2, wall (-width2), vertWalls height2,  vertWalls (-height2)]
      where 
        width2  = fromIntegral width / 2
        height2 = fromIntegral height / 2

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 22 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)



-- --  ===================

  -- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds * 10
    y' = y + vy * seconds * 10


        -- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- -- | Update the game by moving the ball.
-- -- Ignore the ViewPort argument.
-- update :: ViewPort -> Float -> PongGame -> PongGame 
-- update _ = moveBall 


-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
-- wallBounce :: PongGame -> PongGame


type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallHorizCollision :: Position -> Radius -> Bool 
wallHorizCollision (x, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2
    -- leftCollision   = x + radius >=  fromIntegral height / 2
    -- rightCollision  = x + radius >=  fromIntegral height / 2
wallVertCollision :: Position -> Radius -> Bool 
wallVertCollision (x, _) radius = leftCollision || rightCollision 
  where
    -- topCollision    = y - radius <= -fromIntegral width / 2 
    -- bottomCollision = y + radius >=  fromIntegral width / 2
    rightCollision   = x + radius >=  fromIntegral height / 2
    leftCollision  = x - radius <=  -fromIntegral height / 2



wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx', vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallHorizCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy
    vx' = if wallVertCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vx
           else
            -- Do nothing. Return the old velocity.
            vx


-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
-- paddleBounce :: PongGame -> PongGame

-- paddleCollision :: PongGame -> Position -> Radius -> Bool
-- paddleCollision game (x, y) radius = leftPaddleCol || rightPaddleCol 
--   where
--     leftPaddleCol   = 
--     rightPaddleCol  =



-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game =
  game { ballLoc = (0, 0) }

-- Do nothing for all other events.
handleKeys _ game = game




-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update  seconds = wallBounce . moveBall seconds
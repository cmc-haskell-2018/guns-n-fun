{-# LANGUAGE MultiWayIf #-}

module GunsAndFun(run) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

import Data.Set (Set, member, empty, notMember, insert, delete, fromList)

run :: IO ()
run = play window background fps initialState render catchKey update

width, height, offset :: Int
width = 1000
height = 600
offset = 100

maxvx, maxvy :: Float
maxvx = 200
maxvy = 400

allKeys :: [ ( Key, GameState -> GameState ) ]
allKeys = [
    ( (Char 'w'), handleW ),
    ( (Char 'a'), handleA ),
    ( (Char 'd'), handleD ), 
    ( (Char 'q'), shoot   )
    ]

permissibleKeys :: Set Key
permissibleKeys = fromList $ map fst allKeys

window :: Display
window = InWindow "GunsNFun" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

eps :: Float
eps = 4

ge :: Float
ge = 800

numbOfBullets :: Int
numbOfBullets = 350

data Block = Block {
    x1, x2, y1, y2 :: Float,
    blockColor :: Color
    }

data Player = Player { 
    block  :: Block,
    vx, vy :: Float
    }

type KeyboardState = Set Key
type Bullets = [Bullet]

--player
--[block]
--[bullet]
--keyboardState
--time?
--score?
data GameState = GameState {
    player1 :: Player,
    blocks  :: [Block],
    kbState :: KeyboardState,
    bullets1 :: Bullets
}
data Bullet = Nothing | Bullet
  {
    -- buLoc :: (Float, Float)
     blocX :: Float
    , blocY :: Float
    , bSpeedx :: Float
    , bSpeedy :: Float

  } deriving Show


initialState :: GameState
initialState = GameState {
    player1  = initPlayer,
    blocks  = initBlocks,
    kbState = (empty :: Set Key),
    bullets1 =   [GunsAndFun.Nothing ]

}

initPlayer :: Player
initPlayer = Player {
    block = Block {
        x1 = (-10),
        y1 = (-10),
        x2 = 10,
        y2 = 10,
        blockColor = red
    },
    vx = 0,
    vy = 0
}

initBlocks :: [Block]
initBlocks = [
    (Block (-400) 400 (-250) (-200) blue),
    (Block (-400) (-350) (-200) 250 blue),
    (Block 350 400 (-200) 250 blue),
    (Block (-100) 100 (-150) (-140) blue)
    ]


initialBullets :: Player -> Bullet
initialBullets player1  = Bullet
  {
    -- buLoc  = (pLoc player1)
     blocX = ((x1 (block player1)) + (x2 (block player1))) / 2
    ,blocY = ((y1 (block player1)) + (y2 (block player1))) / 2
    , bSpeedx = (vx player1) + (if (vx player1) >= 0 then 15 else (-15))
    , bSpeedy = 0 --(vy player1)

  }
addBullets :: Player -> [Bullet] -> [Bullet]
addBullets player1 bullets =   [(initialBullets player1)] ++ (take numbOfBullets bullets )


render :: GameState -> Picture
render game =
    pictures (bull ++ (playerSprite: blockList))
    where
        playerSprite = drawBlock . block . player1 $ game
        blockList    = map drawBlock $ blocks $ game
        bull = renderBulletsList (bullets1 game)


renderBulletsList :: Bullets -> [Picture]
renderBulletsList [] = [Blank]
renderBulletsList bullets = fmap renderBullets bullets

renderBullets :: Bullet -> Picture
renderBullets (GunsAndFun.Nothing) = Blank --pictures [translate (-10) 10 $ color (light green) $ rectangleSolid 10 20]
renderBullets bullets = pictures [translate (blocX bullets) (blocY bullets) $ color (dark green) $ rectangleSolid 7 7]

drawBlock :: Block -> Picture
drawBlock (Block x1 x2 y1 y2 blockColor) =
    translate ((x1 + x2) / 2) ((y1 + y2) / 2) $ color blockColor $ rectangleSolid (x2 - x1) (y2 - y1)

catchKey :: Event -> GameState -> GameState
catchKey (EventKey key keyState _ _) game =
     if | notMember key permissibleKeys -> game
        | keyState == Up   -> game { kbState = delete key (kbState game) }
        | keyState == Down -> game { kbState = insert key (kbState game) }
catchKey _ game = game

update :: Float -> GameState -> GameState
update seconds game = 
    movePlayer seconds $ handleCollisions $ handleKeys $ setVxToZero game
     where
        setVxToZero g = setvx 0 g
--проверить возможность прыжка
--обработать клавиши
--установить скорости
--передвинуть игрока

nonZeroIntersection :: (Float, Float) -> (Float, Float) -> Bool
nonZeroIntersection (a, b) (c, d) = 
    if  | (c >= a) && (c < b) -> True
        | (a >= c) && (a < d) -> True
        | otherwise -> False

downCollision :: Block -> Block -> Bool --проверить, касается ли нижняя сторона первого прямоугольника верхней стороны второго
downCollision (Block ax1 ax2 ay1 ay2 _) (Block bx1 bx2 by1 by2 _) = 
    ((abs (ay1 - by2)) < eps) && (nonZeroIntersection (ax1, ax2) (bx1, bx2))
        

upperCollision :: Block -> Block -> Bool --проверить, касается ли верхняя сторона первого прямоугольника нижней стороны второго
upperCollision a b = downCollision b a


rightCollision :: Block -> Block -> Bool --проверить, касается ли правая сторона первого прямоугольника левой стороны второго
rightCollision (Block ax1 ax2 ay1 ay2 _) (Block bx1 bx2 by1 by2 _) = 
    ( ( abs (ax2 - bx1) ) < eps ) && (nonZeroIntersection (ay1, ay2) (by1, by2) )

leftCollision :: Block -> Block -> Bool
leftCollision a b = rightCollision b a


canJump :: GameState -> Bool
canJump game = 
    foldl1 (||) $ map (downCollision player) blockList
        where
            player = block . player1 $ game
            blockList = blocks game

handleW :: GameState -> GameState
handleW game = 
    if (canJump game) then jumpPlayer game
        else game

handleA :: GameState -> GameState
handleA game = game { player1 = move (player1 game) }
    where 
        move player = player {
            vx = if dpressed then 0 else (-maxvx)
        }
        dpressed = member (Char 'd') (kbState game)

handleD :: GameState -> GameState
handleD game = game { player1 = move (player1 game) }
    where
        move player = player {
            vx = if apressed then 0 else maxvx
        }
        apressed = member (Char 'a') (kbState game)


shoot :: GameState -> GameState
shoot game = game{ bullets1 = shooting  (bullets1 game) }
    where 
        shooting bullets =   (addBullets (player1 game))  (bullets1 game) 
        -- qpressed = member (Char 'q') (kbState game)


jumpPlayer :: GameState -> GameState 
jumpPlayer game = game { player1 = jump (player1 game) }
    where 
        jump player = player {
            vy = maxvy
        }

handleKeys :: GameState -> GameState
handleKeys game = 
    foldl (\a b -> b $ a) game $ map snd $ filter (\(a, b) -> member a (kbState game)) allKeys


handleCollisions :: GameState -> GameState
handleCollisions game = 
    foldl (\a b -> b $ a) game $ map snd $ filter fst myList
        where
            blockList = blocks game
            player = block . player1 $ game
            downCol  = foldl1 (||) $ map (downCollision player)  blockList
            upperCol = foldl1 (||) $ map (upperCollision player) blockList
            leftCol  = foldl1 (||) $ map (leftCollision player)  blockList
            rightCol = foldl1 (||) $ map (rightCollision player) blockList
            oldvx = vx . player1 $ game
            oldvy = vy . player1 $ game
            myList = [
                (downCol,  setvy ( max 0 (vy . player1 $ game) )),
                (upperCol, setvy ( min 0 (vy . player1 $ game) )),
                (leftCol,  setvx ( max 0 (vx . player1 $ game) )),
                (rightCol, setvx ( min 0 (vx . player1 $ game) ))
                ]

setvx :: Float -> GameState -> GameState
setvx vx' game = game { player1 = f (player1 game) }
    where
        f player = player { vx = vx' }

setvy :: Float -> GameState -> GameState
setvy vy' game = game { player1 = f (player1 game) }
    where
        f player = player { vy = vy' }

movePlayer :: Float -> GameState -> GameState
movePlayer seconds game = game { player1 = newPlayer,    
    bullets1  = (moveBullets seconds game (bullets1 game)) }
    where
        newPlayer = (player1 game) { block = newBlock, vy = vy' }
        newBlock = (block (player1 game)) {x1 = x1', x2 = x2', y1 = y1', y2 = y2'}
        x1' = (x1.block.player1$game) + (vx.player1$game) * seconds
        x2' = (x2.block.player1$game) + (vx.player1$game) * seconds
        y1' = (y1.block.player1$game) + (vy.player1$game) * seconds
        y2' = (y2.block.player1$game) + (vy.player1$game) * seconds
        vy' = if canJump game then (vy.player1$game) else (vy.player1$game) - ge * seconds

moveBullets :: Float -> GameState -> Bullets -> Bullets
moveBullets _ _ [] = []
moveBullets seconds game (bullet : bullets) =  (move bullet) : (moveBullets seconds game  bullets)
    where
        move bull = case bull of Bullet{} -> Bullet{
            blocX = x'
            , blocY = y'
            , bSpeedx = vx
            , bSpeedy = vy
          }; (GunsAndFun.Nothing) -> GunsAndFun.Nothing
        x = (blocX (bullet ))
        y = (blocY (bullet ))
        vx = bSpeedx (bullet )
        vy = bSpeedy (bullet )
        x' = x + vx * seconds * 10
        y' = y + vy * seconds * 10
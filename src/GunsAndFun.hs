{-# LANGUAGE MultiWayIf #-}

module GunsAndFun(main) where

--import Graphics.Gloss
--import Graphics.Gloss.Data.Vector
--import Graphics.Gloss.Geometry.Line
--import Graphics.Gloss.Interface.Pure.Game

import Data.Set (Set, member, notMember, insert, delete, fromList)

main :: IO ()
main = play window background fps initialState render catchKey update

width, height, offset :: Int
width = 1000
height = 600
offset = 100

allKeys :: [ ( Key, GameState -> GameState ) ]
allKeys = [
	( (Char 'w'), handleW ),
	( (Char 'a'), handleA ),
	( (Char 'd'), handleD )
]

permissibleKeys :: Set Key
permissibleKeys = fromList $ map fst allKeys

window :: Display
window = InWindow "GunsNFun" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

eps :: Double
eps = 10

ge :: Double
ge = 1

data Block = Block {
	x1, x2, y1, y2 :: Double,
	blockColor :: Color
}

data Player = Player { 
	block  :: Block,
	vx, vy :: Double
}

type KeyboardState = Set Key

--player
--[block]
--[bullet]
--keyboardState
--time?
--score?
data GameState = GameState {
	player1 :: Player,
	blocks  :: [Block],
	kbState :: KeyboardState
}

initialState :: GameState
initialState = GameState {
	player1  = initPlayer,
	blocks  = initBlocks,
	kbState = (empty :: Set Key)
}

initPlayer :: Player
initPlayer = Player {
	block = Block {
		x1 = (-10),
		y1 = (-10),
		x2 = 10,
		y2 = 10,
		color = red
	},
	vx = 0,
	vy = 0
}

initBlocks :: [Block]
initBlocks = [
	(Block (-400) 400 (-250) (-200) blue),
	(Block (-400) (-350) (-200) 250 blue),
	(Block 350 400 (-200) 250 blue)
]

render :: GameState -> Picture
render game =
	pictures (playerSprite : blockList)
	where
		playerSprite = drawBlock . block . player1 $ game
		blockList    = map drawBlock $ blocks $ game

drawBlock :: Block -> Picture
drawBlock (Block x1 x2 y1 y2 blockColor) =
	translate x1 y1 $ color blockColor $ rectangleSolid (x2 - x1) (y2 - y1)

catchKey :: Event -> GameState -> GameState
catchKey (EventKey key keyState _ _) game =
	 if | notMember key permissibleKeys -> game
		| keyState == Up   -> game { kbState = delete key (kbState game) }
		| keyState == Down -> game { kbState = insert key (kbState game) }
catchKey _ game = game

update :: Float -> GameState -> GameState
update seconds game = undefined
--проверить возможность прыжка
--обработать клавиши
--установить скорости
--передвинуть игрока

nonZeroIntersection :: (Double, Double) -> (Double, Double) -> Bool
nonZeroIntersection (a, b) (c, d) = 
	if  | (c >= a) && (с < b) -> True
		| (a >= c) && (a < d) -> True
		| otherwise -> False

downCollision :: Block -> Block -> Bool --проверить, касается ли нижняя сторона первого прямоугольника верхней стороны второго
downCollision (Block ax1 ax2 ay1 ay2 _) (Block bx1 bx2 by1 by2 _) = 
	((abs (ay1 - by2)) < eps) && (nonZeroIntersection (ax1, ax2) (bx1 bx2))
		

upperCollision :: Block -> Block -> Bool --проверить, касается ли верхняя сторона первого прямоугольника нижней стороны второго
upperCollision a b = downCollision b a


rightCollision :: Block -> Block -> Bool --проверить, касается ли правая сторона первого прямоугольника левой стороны второго
downCollision (Block ax1 ax2 ay1 ay2 _) (Block bx1 bx2 by1 by2 _) = 
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
	where move player = player {
		vx = (-10)
	}

handleD :: GameState -> GameState
handleD game = game { player1 = move (player1 game) }
	where move player = player {
		vx = 10
	}

jumpPlayer :: GameState -> GameState 
jumpPlayer game = game { player1 = jump (player1 game) }
	where 
		jump player = player {
			vy = 10
		}

handleKeys :: GameState -> GameState
handleKeys game = 
	foldl (\a b -> b $ a) game $ map snd $ filter (\(a, b) -> member a (kbState game)) allKeys

handleCollisions :: GameState -> GameState
handleCollisions game = 

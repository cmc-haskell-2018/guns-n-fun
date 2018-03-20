{-# LANGUAGE MultiWayIf #-}

module GunsAndFun(run) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

import Interface
import Types

import Data.Set (Set, member, empty, notMember, insert, delete, fromList)

-- | Главная функция
run :: IO ()
run = do
  images <- loadImages
  play window background fps initialState (render images) catchKey update


-- | Параметры для функции InWindow
width, height, offset :: Int
width = 1000
height = 600
offset = 100


-- | Максимальные скорости игроков
maxvx, maxvy :: Float
maxvx = 200
maxvy = 400


-- | Ассоциативный список из клавиш, нажатие на которые даст эффект
-- Каждой клавише соответствует функция, принимающая количество секунд с предыдущего кадра
-- (нужно, например, для обработки прыжка) и текущее игровое состояние,
-- возвращает новое игровое состояние
allKeys :: [ ( Key, GameState -> GameState ) ]
allKeys = [
    ( (Char 'w'), handleW ),
    ( (Char 'a'), handleA ),
    ( (Char 'd'), handleD ),
    ( (Char 'q'), shoot   )
    ]


-- | Множество клавиш, нажатие на которые даст эффект
-- Нужно для функции catchKey, чтобы понять, нужно ли игнорировать нажатие
permissibleKeys :: Set Key
permissibleKeys = fromList $ map fst allKeys


-- | Настройки окна
window :: Display
window = InWindow "GunsNFun" (width, height) (offset, offset)


-- | Цвет фона
background :: Color
background = white


-- | ФПС
fps :: Int
fps = 60


-- | Погрешность для Float
-- Используется в коллизиях, чтобы не сравнивать с нулём
eps :: Float
eps = 0.001


-- | Ускорение свободного падения
ge :: Float
ge = 800


-- | Объект - координаты и скорость
-- Является составной частью классов Block, Player, Bullet
-- Коллизии считаются именно для двух Object-ов
data Object = Object {
    x1, x2, y1, y2 :: Float,
    vx, vy         :: Float
    }
{-numbOfBullets :: Int
  numbOfBullets = 10

  data Block = Block {
      x1, x2, y1, y2 :: Float,
      blockColor :: Color
      }

  data Player = Player {
      block  :: Block,
      vx, vy :: Float
      }-}

-- | Блок - элемент игрового поля
-- Поля, задающие скорость, должны быть равны нулю
data Block = Block { bobj :: Object, blockColor :: Color }


-- | Игрок
data Player = Player { pobj :: Object, playerColor :: Color }


-- | Состояние клавиатуры на текущий кадр
-- Множество нажатых клавиш
type KeyboardState = Set Key
type Bullets = [Bullet]


-- | Игровое состояние
data GameState = GameState {
    player1  :: Player,
    blocks   :: [Block],
    kbState  :: KeyboardState,
    secsLeft :: Float, -- ^ Поле, куда запоминается значение seconds из update
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
    blocks   = initBlocks,
    kbState  = (empty :: Set Key),
    secsLeft = 0,
    bullets1 =   [GunsAndFun.Nothing ]

}

initPlayer :: Player
initPlayer = Player {
    pobj = Object {
        x1 = (-10),
        y1 = (-10),
        x2 = 10,
        y2 = 10,
        vx = 0,
        vy = 0
    },
    playerColor = dark red
}

initBlocks :: [Block]
initBlocks = [
    (Block (Object (-400) 400 (-250) (-200) 0 0) blue),
    (Block (Object (-400) (-350) (-200) 250 0 0) blue),
    (Block (Object 350 400 (-200) 250 0 0) blue),
    (Block (Object (-100) 100 (-150) (-140) 0 0) blue)
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


render :: Images -> GameState -> Picture
render images game = do
  pictures (bull ++((drawSprite images (ceiling (secsLeft game)) (player1 $ game)) : blockList))
	  where
		  blockList    = map drawBlock $ blocks $ game
      bull = renderBulletsList (bullets1 game)

renderBulletsList :: Bullets -> [Picture]
renderBulletsList [] = [Blank]
renderBulletsList bullets = fmap renderBullets bullets

renderBullets :: Bullet -> Picture
renderBullets (GunsAndFun.Nothing) = Blank --pictures [translate (-10) 10 $ color (light green) $ rectangleSolid 10 20]
renderBullets bullets = pictures [translate (blocX bullets) (blocY bullets) $ color (dark green) $ rectangleSolid 7 7]


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


drawBlock :: Block -> Picture
drawBlock (Block (Object x1 x2 y1 y2 _ _) blockColor) =
    translate ((x1 + x2) / 2) ((y1 + y2) / 2) $ color blockColor $ rectangleSolid (x2 - x1) (y2 - y1)

drawPlayer :: Player -> Picture
drawPlayer (Player object playerColor) = drawBlock Block { bobj = object, blockColor = playerColor }

drawSprite :: Images -> Integer -> Player -> Picture
drawSprite images sec (Player (Object x1 x2 y1 y2 _ _) blockColor) =
  translate ((x1 + x2) / 2) ((y1 + y2) / 2 + 30) image
  where
    modd = mod (ceiling x1) 10
    image = case modd of
      1  -> (image41 images)
      2  -> (image42 images)
      3  -> (image43 images)
      4  -> (image44 images)
      5  -> (image45 images)
      6  -> (image46 images)
      7  -> (image47 images)
      _   -> (image48 images)


catchKey :: Event -> GameState -> GameState
catchKey (EventKey key keyState _ _) game =
     if | notMember key permissibleKeys -> game
        | keyState == Up   -> game { kbState = delete key (kbState game) }
        | keyState == Down -> game { kbState = insert key (kbState game) }
catchKey _ game = game

update :: Float -> GameState -> GameState
update seconds game = 
    movePlayer1 $ handleCollisions $ handleKeys $ setVxToZero $ rememberSeconds seconds game
     where
        setVxToZero g = setvx 0 g
--проверить возможность прыжка
--обработать клавиши
--установить скорости
--передвинуть игрока


rememberSeconds :: Float -> GameState -> GameState
rememberSeconds seconds game = game {secsLeft = seconds}


nonZeroIntersection :: (Float, Float) -> (Float, Float) -> Bool
nonZeroIntersection (a, b) (c, d) = 
    if  | (c >= a) && (c < b) -> True
        | (a >= c) && (a < d) -> True
        | otherwise -> False

downCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли нижняя сторона первого прямоугольника верхней стороны второго
--(Bool, Float) - произойдёт ли коллизия, и если да, то через сколько секунд
downCollision (Object ax1 ax2 ay1 ay2 avx avy) (Object bx1 bx2 by1 by2 bvx bvy) =
     if | (abs dist) < eps -> if (nonZeroIntersection (ax1, ax2) (bx1, bx2)) then (True, 0) else (False, 0) -- если вплотную
        | vy > -eps  -> (False, 0) -- отдаляются
        | ay1 < by2  -> (False, 0) -- первый не выше второго
        | nonZeroIntersection (ax1', ax2') (bx1, bx2) -> (True, time)
        | otherwise  -> (False, 0)
        where
            dist = ay1 - by2
            vx   = avx - bvx
            vy   = avy - bvy
            time = abs (dist / vy)
            ax1' = ax1 + vx * time
            ax2' = ax2 + vx * time


upperCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли верхняя сторона первого прямоугольника нижней стороны второго
upperCollision a b = downCollision b a


rightCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли правая сторона первого прямоугольника левой стороны второго
--(Bool, Float) - произойдёт ли коллизия, и если да, то через сколько секунд
rightCollision (Object ax1 ax2 ay1 ay2 avx avy) (Object bx1 bx2 by1 by2 bvx bvy) =
     if | (abs dist) < eps -> if (nonZeroIntersection (ay1, ay2) (by1, by2)) then (True, 0) else (False, 0) -- если вплотную
        | vx < eps   -> (False, 0) -- отдаляются
        | ax2 > bx1  -> (False, 0) -- первый правее второго
        | nonZeroIntersection (ay1', ay2') (by1, by2) -> (True, time)
        | otherwise  -> (False, 0)
        where
            dist = bx1 - ax2
            vx   = avx - bvx
            vy   = avy - bvy
            time = abs (dist / vx)
            ay1' = ay1 + vy * time
            ay2' = ay2 + vy * time

leftCollision :: Object -> Object -> (Bool, Float)
leftCollision a b = rightCollision b a


canJump :: GameState -> Bool
canJump game = 
    (downList /= []) && ((minimum valuesList) <= seconds)
        where
            seconds = secsLeft game
            valuesList = map snd downList
            downList = filter fst $ map (downCollision player) blockList -- [(True, Float)] или []
            player = pobj . player1 $ game -- Object
            blockList = map bobj $ blocks game -- [Object]



handleW :: GameState -> GameState
handleW game = 
    if (canJump game) then setvy maxvy game else game


handleA :: GameState -> GameState
handleA game = setvx (if dpressed then 0 else (-maxvx)) game
    where dpressed = member (Char 'd') (kbState game)


handleD :: GameState -> GameState
handleD game = setvx (if apressed then 0 else maxvx) game
    where apressed = member (Char 'a') (kbState game)


handleKeys :: GameState -> GameState
handleKeys game = 
    foldl (\a b -> b $ a) game $ map snd $ filter (\(a, b) -> member a (kbState game)) allKeys
        where
            seconds = secsLeft game

shoot :: GameState -> GameState
shoot game = game{ bullets1 = shooting  (bullets1 game) }
    where
        shooting bullets =   (addBullets (player1 game))  (bullets1 game)
        -- qpressed = member (Char 'q') (kbState game)



handleCollisions :: GameState -> GameState
handleCollisions game = 
    foldl (\a b -> b $ a) game $ map snd $ filter fst myList
        where
            seconds = secsLeft game
            blockList = map bobj $ blocks game
            player = pobj . player1 $ game
            downValues   = map snd $ filter fst $ map (downCollision  player) blockList -- [Float] или []
            upperValues  = map snd $ filter fst $ map (upperCollision player) blockList
            leftValues   = map snd $ filter fst $ map (leftCollision  player) blockList
            rightValues  = map snd $ filter fst $ map (rightCollision player) blockList
            downCol      = downValues  /= [] -- произойдёт ли
            upperCol     = upperValues /= []
            leftCol      = leftValues  /= []
            rightCol     = rightValues /= []
            downTime     = if downCol  then minimum downValues  else 0 -- когда произойдёт
            upperTime    = if upperCol then minimum upperValues else 0
            leftTime     = if leftCol  then minimum leftValues  else 0
            rightTime    = if rightCol then minimum rightValues else 0
            oldvx = vx . pobj . player1 $ game
            oldvy = vy . pobj . player1 $ game
            downDist  = if (downTime  <= seconds) then oldvy * downTime  else 0 -- на сколько сдвинуть
            upperDist = if (upperTime <= seconds) then oldvy * upperTime else 0
            leftDist  = if (leftTime  <= seconds) then oldvx * leftTime  else 0
            rightDist = if (rightTime <= seconds) then oldvx * rightTime else 0
            myList = [
                (downCol  && (downTime  <= seconds), (player1addy downDist)  . (setvy ( max 0 oldvy ))),
                    --если на данном кадре будет нижняя коллизия, то игрока пододвинем вплотную
                (upperCol && (upperTime <= seconds), (player1addy upperDist) . (setvy ( min 0 oldvy ))),
                (leftCol  && (leftTime  <= seconds), (player1addx leftDist)  . (setvx ( max 0 oldvx ))),
                (rightCol && (rightTime <= seconds), (player1addx rightDist) . (setvx ( min 0 oldvx )))
                ]


setvx :: Float -> GameState -> GameState
setvx vx' game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (pobj player) { vx = vx' } }


setvy :: Float -> GameState -> GameState
setvy vy' game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (pobj player) { vy = vy' } }


player1addx :: Float -> GameState -> GameState
player1addx 0 game = game
player1addx x game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (pobj player) {x1 = oldx1 + x, x2 = oldx2 + x} }
        oldx1 = x1.pobj.player1$game
        oldx2 = x2.pobj.player1$game


player1addy :: Float -> GameState -> GameState
player1addy 0 game = game
player1addy y game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (pobj player) {y1 = oldy1 + y, y2 = oldy2 + y} }
        oldy1 = y1.pobj.player1$game
        oldy2 = y2.pobj.player1$game


movePlayer1 :: GameState -> GameState
movePlayer1 game = game { player1 = newPlayer }
movePlayer1 game = game { player1 = newPlayer,
    bullets1  = (moveBullets seconds game (bullets1 game)) }

    where
        seconds = secsLeft game
        newPlayer = (player1 game) { pobj = newObject }
        newObject = (pobj.player1$game) {x1 = x1', x2 = x2', y1 = y1', y2 = y2', vy = vy'}
        x1' = (x1.pobj.player1$game) + (vx.pobj.player1$game) * seconds
        x2' = (x2.pobj.player1$game) + (vx.pobj.player1$game) * seconds
        y1' = (y1.pobj.player1$game) + (vy.pobj.player1$game) * seconds
        y2' = (y2.pobj.player1$game) + (vy.pobj.player1$game) * seconds
        vy' = if (canJump game) then (vy.pobj.player1$game) else (vy.pobj.player1$game) - ge * seconds

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
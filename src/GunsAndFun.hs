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
    ( (Char 'd'), handleD )
    ,    ( (Char 'q'), shootQ )
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

-- | Половина размера пуль
bulletSize :: Float
bulletSize = 3

-- | Количество пуль для одного игрока
numbOfBullets :: Int
numbOfBullets = 350

-- | Скорость пуль в игре
bulletspeed:: Float
bulletspeed = 50
-- | Объект - координаты и скорость
-- Является составной частью классов Block, Player, Bullet
-- Коллизии считаются именно для двух Object-ов
data Object = Object {
    x1, x2, y1, y2 :: Float,
    vx, vy         :: Float
    }


-- | Блок - элемент игрового поля
-- Поля, задающие скорость, должны быть равны нулю
data Block = Block { bobj :: Object, blockColor :: Color }


-- | Игрок
data Player = Player { pobj :: Object, playerColor :: Color }

-- | Пуля
data Bullet = Nothing | Bullet { bulletobj :: Object, bulletColor :: Color}

type Bullets = [Bullet]

-- | Класс объектов, внутри которых есть Object
-- Для удобства работы с коллизиями (см. downCollision и т. п.)
class HasObject a where
    getObject :: a -> Object

instance HasObject Object where
    getObject = id


instance HasObject Block where
    getObject = bobj


instance HasObject Player where
    getObject = pobj

instance HasObject Bullet where
    getObject = bulletobj

-- | Состояние клавиатуры на текущий кадр
-- Множество нажатых клавиш
type KeyboardState = Set Key


-- | Игровое состояние
data GameState = GameState {
    player1  :: Player,
    blocks   :: [Block],
    kbState  :: KeyboardState,
    secsLeft :: Float ,-- ^ Поле, куда запоминается значение seconds из update
    bullets1 :: Bullets
}


initialState :: GameState
initialState = GameState {
    player1  = initPlayer,
    blocks   = initBlocks,
    kbState  = (empty :: Set Key),
    secsLeft = 0,
    bullets1 = [GunsAndFun.Nothing]
}

initBullet :: GameState -> Bullet
initBullet game = Bullet {
    bulletobj = Object {
        x1 = (((x1.getObject.player1$game) + (x2.getObject.player1$game)) /2) - bulletSize, 
        y1 = (((y1.getObject.player1$game) + (y2.getObject.player1$game)) /2) - bulletSize, 
        x2 = (((x2.getObject.player1$game) + (x1.getObject.player1$game)) /2) + bulletSize, 
        y2 = (((y2.getObject.player1$game) + (y1.getObject.player1$game)) /2) + bulletSize,  
        vx = (vx.getObject.player1$game) + (if (vx.getObject.player1$game) >= 0 then bulletspeed else (-bulletspeed)), 
        vy = 0
    },
    bulletColor = (light green)
}

initPlayer :: Player
initPlayer = Player {
    pobj = Object {
        x1 = (-14),
        y1 = (-16),
        x2 = 14,
        y2 = 16,
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

render :: Images -> GameState -> Picture
render images game = do
  pictures (bull ++ ((drawSprite images  (player1 $ game)) : blockList))
     where
          bull = map drawBullet $ bullets1 $ game
          blockList = map drawBlock $ blocks $ game

drawBullet :: Bullet -> Picture
drawBullet GunsAndFun.Nothing = Blank
drawBullet (Bullet (Object x1 x2 y1 y2 _ _) bulletColor) = 
    translate ((x1 + x2) / 2) ((y1 + y2) / 2) $ color bulletColor $ rectangleSolid (x2 - x1) (y2 - y1)

drawBlock :: Block -> Picture
drawBlock (Block (Object x1 x2 y1 y2 _ _) blockColor) =
    translate ((x1 + x2) / 2) ((y1 + y2) / 2) $ color blockColor $ rectangleSolid (x2 - x1) (y2 - y1)


drawPlayer :: Player -> Picture
drawPlayer (Player object playerColor) = drawBlock Block { bobj = object, blockColor = playerColor }


drawSprite :: Images -> Player -> Picture
drawSprite images (Player (Object x1 x2 y1 y2 vx vy) blockColor) =
  translate ((x1 + x2) / 2) ((y1 + y2) / 2) image
  where
    modx = mod (ceiling x1) 80
    modvy = mod (floor vy) 1000
    image = case modvy of
      n | n > 0    && n < 100    -> (image21 images)
      n | n >= 100 && n < 200  -> (image22 images)
      n | n >= 200 && n < 300  -> (image23 images)
      n | n >= 300 && n < 400  -> (image24 images)
      n | n >= 400 && n < 500  -> (image25 images)
      n | n >= 500 && n < 600  -> (image26 images)
      n | n >= 600 && n < 700  -> (image27 images)
      n | n >= 700 && n < 800  -> (image28 images)
      n | n >= 800 && n < 900  -> (image29 images)
      n | n >= 900 && n < 1000 -> (image30 images)
      _ -> case modx of
        n | n >= 0  && n < 10  -> (image11 images)
        n | n >= 10 && n < 20  -> (image12 images)
        n | n >= 20 && n < 30  -> (image13 images)
        n | n >= 30 && n < 40  -> (image14 images)
        n | n >= 40 && n < 50  -> (image15 images)
        n | n >= 50 && n < 60  -> (image16 images)
        n | n >= 60 && n < 70  -> (image17 images)
        _                      -> (image18 images)


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


objDownCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли нижняя сторона первого прямоугольника верхней стороны второго
--(Bool, Float) - произойдёт ли коллизия, и если да, то через сколько секунд
objDownCollision (Object ax1 ax2 ay1 ay2 avx avy) (Object bx1 bx2 by1 by2 bvx bvy) =
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


objUpperCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли верхняя сторона первого прямоугольника нижней стороны второго
objUpperCollision a b = objDownCollision b a


objRightCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли правая сторона первого прямоугольника левой стороны второго
--(Bool, Float) - произойдёт ли коллизия, и если да, то через сколько секунд
objRightCollision (Object ax1 ax2 ay1 ay2 avx avy) (Object bx1 bx2 by1 by2 bvx bvy) =
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


objLeftCollision :: Object -> Object -> (Bool, Float)
objLeftCollision a b = objRightCollision b a


-- | Проверяет, есть ли нижняя коллизия
-- На вход подаются два любых объекта класса HasObject
downCollision :: (HasObject a, HasObject b) => a -> b -> (Bool, Float)
downCollision a' b' = objDownCollision (getObject a') (getObject b')


upperCollision :: (HasObject a, HasObject b) => a -> b -> (Bool, Float)
upperCollision a' b' = objUpperCollision (getObject a') (getObject b')


leftCollision :: (HasObject a, HasObject b) => a -> b -> (Bool, Float)
leftCollision a' b' = objLeftCollision (getObject a') (getObject b')


rightCollision :: (HasObject a, HasObject b) => a -> b -> (Bool, Float)
rightCollision a' b' = objRightCollision (getObject a') (getObject b')


canJump :: GameState -> Bool
canJump game =
    (downList /= []) && ((minimum valuesList) <= seconds)
        where
            seconds = secsLeft game
            valuesList = map snd downList
            downList   = filter fst $ map (downCollision player) blockList -- [(True, Float)] или []
            player     = player1 game
            blockList  = blocks game



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


handleCollisions :: GameState -> GameState
handleCollisions game =
    foldl (\a b -> b $ a) game $ map snd $ filter fst myList
        where
            seconds = secsLeft game
            blockList = blocks game
            player = player1 game
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
            oldvx = vx . getObject . player1 $ game
            oldvy = vy . getObject . player1 $ game
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
        f player = player { pobj = (getObject player) { vx = vx' } }


setvy :: Float -> GameState -> GameState
setvy vy' game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (getObject player) { vy = vy' } }

player1addx :: Float -> GameState -> GameState
player1addx 0 game = game
player1addx x game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (getObject player) {x1 = oldx1 + x, x2 = oldx2 + x} }
        oldx1 = x1.getObject.player1$game
        oldx2 = x2.getObject.player1$game


player1addy :: Float -> GameState -> GameState
player1addy 0 game = game
player1addy y game = game { player1 = f (player1 game) }
    where
        f player = player { pobj = (getObject player) {y1 = oldy1 + y, y2 = oldy2 + y} }
        oldy1 = y1.getObject.player1$game
        oldy2 = y2.getObject.player1$game

{-
data Object = Object {
    x1, x2, y1, y2 :: Float,
    vx, vy         :: Float
    }


-- | Блок - элемент игрового поля
-- Поля, задающие скорость, должны быть равны нулю
data Block = Block { bobj :: Object, blockColor :: Color }


-- | Игрок
data Player = Player { pobj :: Object, playerColor :: Color }-}


movePlayer1 :: GameState -> GameState
movePlayer1 game = game { player1 = newPlayer, bullets1 = (moveBullets seconds game (bullets1 game)) }
    where
        seconds = secsLeft game
        newPlayer = (player1 game) { pobj = newObject }
        -- newBullets = (bullets1 game) { bulletobj = newBulletObject}
        newObject = (getObject.player1$game) {x1 = x1', x2 = x2', y1 = y1', y2 = y2', vy = vy'}
        x1' = (x1.getObject.player1$game) + (vx.getObject.player1$game) * seconds
        x2' = (x2.getObject.player1$game) + (vx.getObject.player1$game) * seconds
        y1' = (y1.getObject.player1$game) + (vy.getObject.player1$game) * seconds
        y2' = (y2.getObject.player1$game) + (vy.getObject.player1$game) * seconds
        vy' = if (canJump game) then (vy.getObject.player1$game) else (vy.getObject.player1$game) - ge * seconds
        newBulletObject = (getObject.player1$game) {x1 = x1', x2 = x2', y1 = y1', y2 = y2', vy = vy'}


addBullets :: GameState -> [Bullet] -> [Bullet]
addBullets game bullets =   [(initBullet game)] ++ (take numbOfBullets bullets )


shootQ :: GameState -> GameState
shootQ game = game{ bullets1 = shooting  (bullets1 game) }
    where 
        shooting bullets =   (addBullets game)  (bullets1 game) 
        -- qpressed = member (Char 'q') (kbState game)


moveBullets :: Float -> GameState -> Bullets -> Bullets
moveBullets _ _ [] = []
moveBullets seconds game (bullet : bullets) =  (move bullet) : (moveBullets seconds game  bullets)
    where
        move bull = case bull of Bullet{} -> Bullet{
            bulletobj = newObject,
            bulletColor = (light green)
        }; (GunsAndFun.Nothing) -> GunsAndFun.Nothing
        newObject = (getObject.bulletobj$bullet) {x1 = x1', x2 = x2', y1 = y1', y2 = y2', vy = vy'}
        x1' = (x1.getObject$bullet) + (vx.getObject$bullet) * seconds
        x2' = (x2.getObject$bullet) + (vx.getObject$bullet) * seconds
        y1' = (y1.getObject$bullet) + (vy.getObject$bullet) * seconds
        y2' = (y2.getObject$bullet) + (vy.getObject$bullet) * seconds
        vy' = 0
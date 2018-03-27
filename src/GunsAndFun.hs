{-# LANGUAGE MultiWayIf #-}

module GunsAndFun(run) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy


import Interface
import Types

import Data.Set (Set, member, empty, notMember, insert, delete, fromList)
import Data.List(sortOn)
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
maxvy = 600


-- | Множество клавиш, нажатие на которые даст эффект
-- Нужно для функции catchKey, чтобы понять, нужно ли игнорировать нажатие
permissibleKeys :: Set Key
permissibleKeys = fromList
    [
    (Char 'w'),
    (Char 'a'),
    (Char 'd'),
    (Char 'q'),
    (SpecialKey KeyUp),
    (SpecialKey KeyLeft),
    (SpecialKey KeyRight),
    (SpecialKey KeyCtrlR)
    ]


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
ge = 500

-- | Половина размера пуль
bulletSize :: Float
bulletSize = 3

-- | Количество пуль для одного игрока
numbOfBullets :: Int
numbOfBullets = 150

-- | Скорость пуль в игре
bulletspeed:: Float
bulletspeed = 400

-- | Урон, наносимый пулей
bulletDamage :: Float
bulletDamage = 10

-- | Максимальное здоровье
maxHP :: Float
maxHP = 100

-- | Время до воскрешения
secondsToRespawn :: Float
secondsToRespawn = 1

-- | Размер спрайта игрока - (ширина, высота)
playerSize :: (Float, Float)
playerSize = (28, 32)

-- | Скорострельность игрока (количество пуль в секунду)
rateOfFire :: Float
rateOfFire = 5

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
data Player = Player {
    pobj :: Object,
    playerColor :: Color,
    hp    :: Float,
    alive :: Bool,
    timeToRespawn :: Float,
    respawnPoint  :: (Float, Float),
    turnedRight   :: Bool,
    timeToReload :: Float,
    invisible :: Bool,
    blinkingTime :: Float,
    sameStateTime :: Float
    }


-- | Пуля
data Bullet = Bullet {
    bulletobj :: Object,
    bulletColor :: Color,
    damage :: Float
    }


type Bullets = [Bullet]


-- | Класс объектов, внутри которых есть Object
-- Для удобства работы с коллизиями (см. downCollision и т. п.)
class HasObject a where
    getObject :: a -> Object
    setObject :: Object -> a -> a
    getx1 :: a -> Float
    getx2 :: a -> Float
    gety1 :: a -> Float
    gety2 :: a -> Float
    getvx :: a -> Float
    getvy :: a -> Float
    setx1 :: Float -> a -> a
    setx2 :: Float -> a -> a
    sety1 :: Float -> a -> a
    sety2 :: Float -> a -> a
    setvx :: Float -> a -> a
    setvy :: Float -> a -> a


instance HasObject Object where
    getObject = id
    setObject newObject _ = newObject
    getx1 = x1
    getx2 = x2
    gety1 = y1
    gety2 = y2
    getvx = vx
    getvy = vy
    setx1 newx1 this = this { x1 = newx1 }
    setx2 newx2 this = this { x2 = newx2 }
    sety1 newy1 this = this { y1 = newy1 }
    sety2 newy2 this = this { y2 = newy2 }
    setvx newvx this = this { vx = newvx }
    setvy newvy this = this { vy = newvy }


instance HasObject Block where
    getObject = bobj
    setObject newObject this = this { bobj = newObject }
    getx1 = x1.bobj
    getx2 = x2.bobj
    gety1 = y1.bobj
    gety2 = y2.bobj
    getvx = vx.bobj
    getvy = vy.bobj
    setx1 newx1 this = this { bobj = setx1 newx1 $ bobj this }
    setx2 newx2 this = this { bobj = setx2 newx2 $ bobj this }
    sety1 newy1 this = this { bobj = sety1 newy1 $ bobj this }
    sety2 newy2 this = this { bobj = sety2 newy2 $ bobj this }
    setvx newvx this = this { bobj = setvx newvx $ bobj this }
    setvy newvy this = this { bobj = setvy newvy $ bobj this }


instance HasObject Player where
    getObject = pobj
    setObject newObject this = this { pobj = newObject }
    getx1 = x1.pobj
    getx2 = x2.pobj
    gety1 = y1.pobj
    gety2 = y2.pobj
    getvx = vx.pobj
    getvy = vy.pobj
    setx1 newx1 this = this { pobj = setx1 newx1 $ pobj this }
    setx2 newx2 this = this { pobj = setx2 newx2 $ pobj this }
    sety1 newy1 this = this { pobj = sety1 newy1 $ pobj this }
    sety2 newy2 this = this { pobj = sety2 newy2 $ pobj this }
    setvx newvx this = this { pobj = setvx newvx $ pobj this }
    setvy newvy this = this { pobj = setvy newvy $ pobj this }


instance HasObject Bullet where
    getObject = bulletobj
    setObject newObject this = this { bulletobj = newObject }
    getx1 = x1.bulletobj
    getx2 = x2.bulletobj
    gety1 = y1.bulletobj
    gety2 = y2.bulletobj
    getvx = vx.bulletobj
    getvy = vy.bulletobj
    setx1 newx1 this = this { bulletobj = setx1 newx1 $ bulletobj this }
    setx2 newx2 this = this { bulletobj = setx2 newx2 $ bulletobj this }
    sety1 newy1 this = this { bulletobj = sety1 newy1 $ bulletobj this }
    sety2 newy2 this = this { bulletobj = sety2 newy2 $ bulletobj this }
    setvx newvx this = this { bulletobj = setvx newvx $ bulletobj this }
    setvy newvy this = this { bulletobj = setvy newvy $ bulletobj this }


-- | Состояние клавиатуры на текущий кадр
-- Множество нажатых клавиш
type KeyboardState = Set Key


-- | Игровое состояние
data GameState = GameState {
    player1  :: Player,
    player2  :: Player,
    blocks   :: [Block],
    kbState  :: KeyboardState,
    secsLeft :: Float ,-- ^ Поле, куда запоминается значение seconds из update
    bullets1 :: Bullets
}


initialState :: GameState
initialState = GameState {
    player1  = initPlayer1,
    player2  = initPlayer2,
    blocks   = initBlocks,
    kbState  = (empty :: Set Key),
    secsLeft = 0,
    bullets1 = []
}


initPlayer1 :: Player
initPlayer1 = Player {
    pobj = Object {
        x1 = (-200),
        y1 = 0,
        x2 = (-200) + (fst playerSize),
        y2 = 0 + (snd playerSize),
        vx = 0,
        vy = 0
    },
    playerColor = dark red,
    hp = maxHP,
    alive = True,
    timeToRespawn = 0,
    respawnPoint = ((-200), 0),
    turnedRight = True,
    timeToReload = 0,
    invisible = False,
    blinkingTime = 0,
    sameStateTime = 0
}


initPlayer2 :: Player
initPlayer2 = Player {
    pobj = Object {
        x1 = 200,
        y1 = 0,
        x2 = 200 + (fst playerSize),
        y2 = 0 + (snd playerSize),
        vx = 0,
        vy = 0
    },
    playerColor = dark red,
    hp = maxHP,
    alive = True,
    timeToRespawn = 0,
    respawnPoint = (200, 0),
    turnedRight = False,
    timeToReload = 0,
    invisible = False,
    blinkingTime = 0,
    sameStateTime = 0

}


initBlocks :: [Block]
initBlocks = [
    (Block (Object (-400) 400 (-250) (-200) 0 0) blue),
    (Block (Object (-400) (-350) (-200) 250 0 0) blue),
    (Block (Object 350 400 (-200) 250 0 0) blue),
    (Block (Object (-100) 100 (-150) (-140) 0 0) blue)
    ]


render :: Images -> GameState -> Picture
render images game = pictures list''
        where
            list   = (drawPlayer1HP game) : (drawPlayer2HP game) : bulletList ++ blockList
            list'  = if ((alive (player1 game)) && (not (invisible (player1 game)))) then (sprite1 : list) else list
            list'' = if ((alive (player2 game)) && (not (invisible (player2 game)))) then (sprite2 : list') else list'
            sprite1 = drawSprite images 1 (player1 game)
            sprite2 = drawSprite images 2 (player2 game)
            bulletList = map drawBullet (bullets1 game)
            blockList  = map drawBlock  (blocks game)
            --bgpicture = drawBackground

--drawBackground :: Picture
--drawBackground = translate 0 0 pict
--  where
--    pict1 = Just loadJuicyPNG "png/bg.png"
--    pict = scale 2 2 pict1

drawPlayer1HP :: GameState -> Picture
drawPlayer1HP game = pictures list
	where
		isAlive = alive.player1$game
		list = if isAlive then (fill : border : []) else (border : [])
		len = 100
		h = 10
		p1hp = hp.player1$game

		border = translate (-400) 270 $ color red $ rectangleWire len h
		fill   = translate (-400 - len/2*(1 - p1hp/maxHP)) 270 $ color red $ rectangleSolid (len*p1hp/maxHP) h

drawPlayer2HP :: GameState -> Picture
drawPlayer2HP game = pictures list
	where
		isAlive = alive.player2$game
		list = if isAlive then (fill : border : []) else (border : [])
		len = 100
		h = 10
		p2hp = hp.player2$game

		border = translate 400 270 $ color (dark green) $ rectangleWire len h
		fill   = translate (400 - len/2*(1 - p2hp/maxHP)) 270 $ color (dark green) $ rectangleSolid (len*p2hp/maxHP) h


drawBullet :: Bullet -> Picture
drawBullet bullet =
    translate ((x1' + x2') / 2) ((y1' + y2') / 2) $ color (bulletColor bullet) $ rectangleSolid (x2' - x1') (y2' - y1')
        where
            x1' = (getx1 bullet)
            x2' = (getx2 bullet)
            y1' = (gety1 bullet)
            y2' = (gety2 bullet)


drawBlock :: Block -> Picture
drawBlock (Block (Object x1' x2' y1' y2' _ _) blockColor') =
    translate ((x1' + x2') / 2) ((y1' + y2') / 2) $ color blockColor' $ rectangleSolid (x2' - x1') (y2' - y1')

{-
data Player = Player {
    pobj :: Object,
    playerColor :: Color,
    hp    :: Float,
    alive :: Bool,
    timeToRespawn :: Float,
    respawnPoint  :: (Float, Float),
    turnedRight   :: Bool
    }
-}

drawSprite :: Images -> Integer -> Player -> Picture
drawSprite images num (Player (Object x1' x2' y1' y2' vx' vy') blockColor' _ _ _ _ tr _ _ _ _) =
  translate ((x1' + x2') / 2) ((y1' + y2') / 2) image
  where
    modx = mod (floor x1') 80
    modvx = mod (floor vx') 1000

    image = case num of
      n | n == 1 -> case tr of
        True -> case vy' of
          n | n > 40              && n < maxvy / 5      -> (image21 images)
          n | n >= maxvy / 5      && n < 2 * maxvy / 5  -> (image22 images)
          n | n >= 2 * maxvy / 5  && n < 3 * maxvy / 5  -> (image23 images)
          n | n >= 3 * maxvy / 5  && n < 4 * maxvy / 5  -> (image24 images)
          n | n >= 4 * maxvy / 5  && n < maxvy          -> (image25 images)
          n | n < -40             && n > -maxvy / 5     -> (image26 images)
          n | n <= -maxvy / 5     && n > -2 * maxvy / 5 -> (image27 images)
          n | n <= -2 * maxvy / 5 && n > -3 * maxvy / 5 -> (image28 images)
          n | n <= -3 * maxvy / 5 && n > -4 * maxvy / 5 -> (image29 images)
          n | n <= -4 * maxvy / 5 && n > -maxvy         -> (image30 images)
          _ -> case modvx of
            n | n == 0                                  -> (image1 images)
            _ -> case modx of
              n | n == 0                                -> (image1 images)
              n | n > 0  && n < 10                      -> (image11 images)
              n | n >= 10 && n < 20                     -> (image12 images)
              n | n >= 20 && n < 30                     -> (image13 images)
              n | n >= 30 && n < 40                     -> (image14 images)
              n | n >= 40 && n < 50                     -> (image15 images)
              n | n >= 50 && n < 60                     -> (image16 images)
              n | n >= 60 && n < 70                     -> (image17 images)
              _                                         -> (image18 images)
        _ -> case vy' of
          n | n > 40               && n < maxvy / 5     -> (image31 images)
          n | n >= maxvy / 5      && n < 2 * maxvy / 5  -> (image32 images)
          n | n >= 2 * maxvy / 5  && n < 3 * maxvy / 5  -> (image33 images)
          n | n >= 3 * maxvy / 5  && n < 4 * maxvy / 5  -> (image34 images)
          n | n >= 4 * maxvy / 5  && n < maxvy          -> (image35 images)
          n | n < -40               && n > -maxvy / 5   -> (image36 images)
          n | n <= -maxvy / 5     && n > -2 * maxvy / 5 -> (image37 images)
          n | n <= -2 * maxvy / 5 && n > -3 * maxvy / 5 -> (image38 images)
          n | n <= -3 * maxvy / 5 && n > -4 * maxvy / 5 -> (image39 images)
          n | n <= -4 * maxvy / 5 && n > -maxvy         -> (image40 images)
          _ -> case modvx of
            n | n == 0                                  -> (image0 images)
            _ -> case modx of
              n | n == 0                                -> (image1 images)
              n | n > 0  && n < 10                      -> (image41 images)
              n | n >= 10 && n < 20                     -> (image42 images)
              n | n >= 20 && n < 30                     -> (image43 images)
              n | n >= 30 && n < 40                     -> (image44 images)
              n | n >= 40 && n < 50                     -> (image45 images)
              n | n >= 50 && n < 60                     -> (image46 images)
              n | n >= 60 && n < 70                     -> (image47 images)
              _                                         -> (image48 images)
      n | n == 2 -> case tr of
        True -> case vy' of
          n | n > 40               && n < maxvy / 5     -> (p2image21 images)
          n | n >= maxvy / 5      && n < 2 * maxvy / 5  -> (p2image22 images)
          n | n >= 2 * maxvy / 5  && n < 3 * maxvy / 5  -> (p2image23 images)
          n | n >= 3 * maxvy / 5  && n < 4 * maxvy / 5  -> (p2image24 images)
          n | n >= 4 * maxvy / 5  && n < maxvy          -> (p2image25 images)
          n | n < -40               && n > -maxvy / 5   -> (p2image26 images)
          n | n <= -maxvy / 5     && n > -2 * maxvy / 5 -> (p2image27 images)
          n | n <= -2 * maxvy / 5 && n > -3 * maxvy / 5 -> (p2image28 images)
          n | n <= -3 * maxvy / 5 && n > -4 * maxvy / 5 -> (p2image29 images)
          n | n <= -4 * maxvy / 5 && n > -maxvy         -> (p2image30 images)
          _ -> case modvx of
            n | n == 0                                  -> (p2image1 images)
            _ -> case modx of
              n | n == 0                                -> (p2image1 images)
              n | n > 0  && n < 10                      -> (p2image11 images)
              n | n >= 10 && n < 20                     -> (p2image12 images)
              n | n >= 20 && n < 30                     -> (p2image13 images)
              n | n >= 30 && n < 40                     -> (p2image14 images)
              n | n >= 40 && n < 50                     -> (p2image15 images)
              n | n >= 50 && n < 60                     -> (p2image16 images)
              n | n >= 60 && n < 70                     -> (p2image17 images)
              _                                         -> (p2image18 images)
        _ -> case vy' of
          n | n > 40               && n < maxvy / 5     -> (p2image31 images)
          n | n >= maxvy / 5      && n < 2 * maxvy / 5  -> (p2image32 images)
          n | n >= 2 * maxvy / 5  && n < 3 * maxvy / 5  -> (p2image33 images)
          n | n >= 3 * maxvy / 5  && n < 4 * maxvy / 5  -> (p2image34 images)
          n | n >= 4 * maxvy / 5  && n < maxvy          -> (p2image35 images)
          n | n < -49               && n > -maxvy / 5   -> (p2image36 images)
          n | n <= -maxvy / 5     && n > -2 * maxvy / 5 -> (p2image37 images)
          n | n <= -2 * maxvy / 5 && n > -3 * maxvy / 5 -> (p2image38 images)
          n | n <= -3 * maxvy / 5 && n > -4 * maxvy / 5 -> (p2image39 images)
          n | n <= -4 * maxvy / 5 && n > -maxvy         -> (p2image40 images)
          _ -> case modvx of
            n | n == 0                                  -> (p2image0 images)
            _ -> case modx of
              n | n == 0                                -> (p2image1 images)
              n | n > 0  && n < 10                      -> (p2image41 images)
              n | n >= 10 && n < 20                     -> (p2image42 images)
              n | n >= 20 && n < 30                     -> (p2image43 images)
              n | n >= 30 && n < 40                     -> (p2image44 images)
              n | n >= 40 && n < 50                     -> (p2image45 images)
              n | n >= 50 && n < 60                     -> (p2image46 images)
              n | n >= 60 && n < 70                     -> (p2image47 images)
              _                                         -> (p2image48 images)




catchKey :: Event -> GameState -> GameState
catchKey (EventKey key keyState _ _) game =
     if | notMember key permissibleKeys -> game
        | keyState == Up   -> game { kbState = delete key (kbState game) }
        | keyState == Down -> game { kbState = insert key (kbState game) }
catchKey _ game = game


update :: Float -> GameState -> GameState
update seconds game = 
    (if (not p2alive) then respawnPlayer2 else id) .
    (if (not p1alive) then respawnPlayer1 else id) .
    moveBullets .
    (if p2alive then handlePlayer2Shooting else id) .
    (if p1alive then handlePlayer1Shooting else id) .
    (if p2alive then movePlayer2 else id) .
    (if p1alive then movePlayer1 else id) .
    (if p2alive then handlePlayer2BlockCollisions else id) .
    (if p1alive then handlePlayer1BlockCollisions else id) .
    (if p2alive then handlePlayer2BulletCollisions else id) .
    (if p1alive then handlePlayer1BulletCollisions else id) .
    (if p2alive then handlePlayer2MovingKeys else id) .
    (if p1alive then handlePlayer1MovingKeys else id) .
    rememberSeconds seconds $
    game
        where
            p1alive = alive (player1 game)
            p2alive = alive (player2 game)


-- | Заносит seconds в GameState и обновляет время игроков до респауна
rememberSeconds :: Float -> GameState -> GameState
rememberSeconds seconds game = game {secsLeft = seconds, player1 = newPlayer1'', player2 = newPlayer2''}
    where
        p1 = player1 game
        p2 = player2 game
        newPlayer1 = if (alive p1) then p1 else p1 { alive = ((timeToRespawn p1) <= seconds),
                                                     timeToRespawn = max 0 ((timeToRespawn p1) - seconds)}
        newPlayer1' = newPlayer1 { timeToReload = max 0 ((timeToReload$player1$game) - seconds)}
        newPlayer1'' = setInvisibility seconds newPlayer1'
        newPlayer2' = newPlayer2 { timeToReload = max 0 ((timeToReload$player2$game)- seconds)}
        newPlayer2 = if (alive p2) then p2 else p2 { alive = ((timeToRespawn p2) <= seconds),
                                                     timeToRespawn = max 0 ((timeToRespawn p2) - seconds)}
        newPlayer2'' = setInvisibility seconds newPlayer2'


setInvisibility :: Float -> Player -> Player
setInvisibility seconds player = 
	if  | (blinkingTime player) < eps  -> player { blinkingTime = 0, invisible = False }
		| (sameStateTime player) < eps -> player { sameStateTime = 0.25,
												   invisible = (not (invisible player)),
												   blinkingTime = (blinkingTime player) - seconds
												 }
		| otherwise -> player { sameStateTime = (sameStateTime player) - seconds,
								blinkingTime  = (blinkingTime player)  - seconds
							  }


-- | Обрабатывает нажатия клавиш w, a, d
handlePlayer1MovingKeys :: GameState -> GameState
handlePlayer1MovingKeys game = game { player1 = newPlayer }
    where
        apressed = member (Char 'a') (kbState game)
        dpressed = member (Char 'd') (kbState game)
        wpressed = member (Char 'w') (kbState game)
        player   = player1 game
        canjump  = canJump player1 game
        seconds  = secsLeft game

        player' = if | apressed && dpressed -> setvx 0 player
                     | apressed             -> turnPlayerLeft  $ setvx (-maxvx) $ player
                     | dpressed             -> turnPlayerRight $ setvx maxvx $ player
                     | otherwise            -> setvx 0 player

        newPlayer = if | canjump && wpressed  -> setvy maxvy player'
                       | canjump              -> setvy 0 player'
                       | otherwise            -> setvy ((getvy player') - ge * seconds) player'


-- | Обрабатывает нажатия стрелок
handlePlayer2MovingKeys :: GameState -> GameState
handlePlayer2MovingKeys game = game { player2 = newPlayer }
    where
        leftpressed  = member (SpecialKey KeyLeft)  (kbState game)
        rightpressed = member (SpecialKey KeyRight) (kbState game)
        uppressed    = member (SpecialKey KeyUp)    (kbState game)
        player       = player2 game
        canjump      = canJump player2 game
        seconds  = secsLeft game
        
        player' = if | leftpressed && rightpressed  -> setvx 0 player
                     | leftpressed              -> turnPlayerLeft  $ setvx (-maxvx) $ player
                     | rightpressed             -> turnPlayerRight $ setvx maxvx $ player
                     | otherwise                -> setvx 0 player

        newPlayer = if | canjump && uppressed  -> setvy maxvy player'
                       | canjump              -> setvy 0 player'
                       | otherwise            -> setvy ((getvy player') - ge * seconds) player'


{-
-- | Обрабатывает столкновения пуль с первым игроком
handlePlayer1BulletCollisions :: GameState -> GameState
handlePlayer1BulletCollisions game = undefined -- нужно реализовать, предполагая, что игрок жив
-- нужно выбрать все пули, которые столкнутся с игроком в этом кадре (с помощью rightCollision и т. п.), получить [Bullet] или [(Float, Bullet)] (см. следующий шаг)
-- необязательно: отсортировать их по времени столкновения с игроком
-- пока игрок жив, нужно для каждой пули:
-- 1) нанести игроку урон (см. causeDamageToPlayer)
-- 2) удалить пулю из списка пуль (см. removeObjectFromList)
-}



-- | Обрабатывает столкновения пуль с первым игроком
handlePlayer1BulletCollisions :: GameState -> GameState
handlePlayer1BulletCollisions game = game { player1 = newPlayer, bullets1 = newBullets }
    where
        seconds = secsLeft game
        blockList = blocks game
        player = player1 game
        oldbullets = bullets1 game
        bulletsWithoutRight = filterBulletsList rightCollision player oldbullets -- [Bullet]
        newBullets = filterBulletsList leftCollision  player bulletsWithoutRight -- [Bullet]
        --
        leftColState  = checkPlayerBulletCollision leftCollision  player seconds oldbullets
        rightColState = checkPlayerBulletCollision rightCollision player seconds oldbullets
        newPlayer = updatePlayerWithBulletCollisions bulletColStateList player
        bulletColStateList = Data.List.sortOn snd (leftColState ++ rightColState)



-- | Удаляет пули, которые коллиируют с игроком в этом кадре
-- Первый аргумент - одна из функций: downCollision, upperCollision, leftCollision, rightCollision
filterBulletsList :: ( Player -> Bullet -> (Bool, Float)) -> Player -> Bullets -> Bullets
filterBulletsList _ _ [] = []
filterBulletsList fcol player (headBullet : tailBullets) =
    if(fst collissionState && (abs(snd collissionState) < (0.05))) then     filterBulletsList fcol player  tailBullets
        else headBullet : filterBulletsList fcol player  tailBullets
    where
        collissionState = fcol player headBullet


checkPlayerBulletCollision :: ( Player -> Bullet -> (Bool, Float)) -> Player -> Float -> Bullets -> [(Bullet, Float)]
checkPlayerBulletCollision _ _ _ [] = []
checkPlayerBulletCollision fcol player seconds (headBullet : tailBullets) =
    if(fst collissionState && ((snd collissionState) < (0.05))) then (headBullet, snd collissionState) : checkPlayerBulletCollision fcol player seconds tailBullets
        else checkPlayerBulletCollision fcol player seconds tailBullets
    where
        -- colTime  = if (fst $ collissionState) then (snd collissionState) else 0-- [Float] или []
        -- isCol   = values /= []
        -- colTime = if isCol then minimum values else 0
        collissionState = fcol player headBullet


-- | Вспомогательная функция для handlePlayer1BulleCollisions. Принимает
-- отсортированный по времени [(Bullet, Float)] - все пули, которые попадут в игрока в этот
-- кадр. И по очереди наносит игроку урон. Когда игрок становится немножечко мёртв,
-- остальные пули игнорируются.
updatePlayerWithBulletCollisions :: [(Bullet, Float)] ->  Player -> Player
updatePlayerWithBulletCollisions [] player0 = player0
updatePlayerWithBulletCollisions ((bullet, time) : tail)  player = if(alive player) then
    newPlayer else player
        where
            newPlayer = updatePlayerWithBulletCollisions tail  (causeDamageToPlayer (damage bullet) player)


{-
-- | Обрабатывает столкновения пуль со вторым игроком
handlePlayer2BulletCollisions :: GameState -> GameState
handlePlayer2BulletCollisions game = undefined -- реализовать аналогично предыдущей функции
-}


-- | Обрабатывает столкновения пуль со вторым игроком
handlePlayer2BulletCollisions :: GameState -> GameState
handlePlayer2BulletCollisions game = game { player2 = newPlayer, bullets1 = newBullets }
    where
        seconds = secsLeft game
        blockList = blocks game
        player = player2 game
        oldbullets = bullets1 game
        bulletsWithoutRight = filterBulletsList rightCollision player oldbullets
        newBullets = filterBulletsList leftCollision  player bulletsWithoutRight
        --
        leftColState  = checkPlayerBulletCollision leftCollision  player seconds oldbullets
        rightColState = checkPlayerBulletCollision rightCollision player seconds oldbullets
        newPlayer = updatePlayerWithBulletCollisions bulletColStateList player
        bulletColStateList = Data.List.sortOn snd (leftColState ++ rightColState)



-- | Наносит игроку заданный урон
causeDamageToPlayer :: Float -> Player -> Player
causeDamageToPlayer dmg player = 
    if | not (alive player) -> player
       | (hp player) <= dmg -> player { alive = False , timeToRespawn = secondsToRespawn }
       | otherwise          -> player { hp = (hp player) - dmg }


-- | Удаляет экземпляр класса HasObject из списка, если он там есть
-- Например, может удалить пулю из списка пуль
removeObjectFromList :: (HasObject a) => a -> [a] -> [a]
removeObjectFromList _ [] = []
removeObjectFromList item (listHead : listTail) = 
    if eqx1 && eqx2 && eqy1 && eqy2 then listTail else (listHead : removeObjectFromList item listTail)
        where
            eqx1 = (abs ((getx1 item) - (getx1 listHead))) < eps
            eqx2 = (abs ((getx2 item) - (getx2 listHead))) < eps
            eqy1 = (abs ((gety1 item) - (gety1 listHead))) < eps
            eqy2 = (abs ((gety2 item) - (gety2 listHead))) < eps


-- | Обрабатывает коллизии первого игрока с блоками, пододвигает его вплотную к блоку,
-- с которым произойдёт столкновение, и изменяет скорости в зависимости от столкновений
handlePlayer1BlockCollisions :: GameState -> GameState
handlePlayer1BlockCollisions game = 
    game { player1 = newPlayer }
        where
            seconds = secsLeft game
            blockList = blocks game
            player = player1 game

            (downCol,  downTime)  = checkPlayerBlocksCollision downCollision  player blockList
            (upperCol, upperTime) = checkPlayerBlocksCollision upperCollision player blockList
            (leftCol,  leftTime)  = checkPlayerBlocksCollision leftCollision  player blockList
            (rightCol, rightTime) = checkPlayerBlocksCollision rightCollision player blockList

            newPlayer = updatePlayerWithBlockCollisions
                            [(downCol,  downTime),
                             (upperCol, upperTime),
                             (leftCol,  leftTime),
                             (rightCol, rightTime)
                             ] seconds player


-- | Обрабатывает коллизии второго игрока с блоками, пододвигает его вплотную к блоку,
-- с которым произойдёт столкновение, и изменяет скорости в зависимости от столкновений
handlePlayer2BlockCollisions :: GameState -> GameState
handlePlayer2BlockCollisions game = 
    game { player2 = newPlayer }
        where
            seconds = secsLeft game
            blockList = blocks game
            player = player2 game

            (downCol,  downTime)  = checkPlayerBlocksCollision downCollision  player blockList
            (upperCol, upperTime) = checkPlayerBlocksCollision upperCollision player blockList
            (leftCol,  leftTime)  = checkPlayerBlocksCollision leftCollision  player blockList
            (rightCol, rightTime) = checkPlayerBlocksCollision rightCollision player blockList

            newPlayer = updatePlayerWithBlockCollisions
                            [(downCol,  downTime),
                             (upperCol, upperTime),
                             (leftCol,  leftTime),
                             (rightCol, rightTime)
                             ] seconds player


-- | Проверяет, есть ли нижняя коллизия между заданным игроком и блоками, и если есть, то возвращает время до скорейшего столкновения
-- Первый аргумент - одна из функций: downCollision, upperCollision, leftCollision, rightCollision
checkPlayerBlocksCollision :: ( Player -> Block -> (Bool, Float)) -> Player -> [Block] -> (Bool, Float)
checkPlayerBlocksCollision fcol player blockList = (isCol, colTime)
    where
        values  = map snd $ filter fst $ map (fcol player) blockList -- [Float] или []
        isCol   = values /= []
        colTime = if isCol then minimum values else 0



-- | Вспомогательная функция для handlePlayer1BlockCollisions
updatePlayerWithBlockCollisions :: [(Bool, Float)] -> Float -> Player -> Player
updatePlayerWithBlockCollisions [(downCol,  downTime),
                                 (upperCol, upperTime),
                                 (leftCol,  leftTime),
                                 (rightCol, rightTime)] seconds player = 
    newPlayer
        where
            downDist  = if (downTime  <= seconds) then (getvy player) * downTime  else 0 -- на сколько сдвинуть
            upperDist = if (upperTime <= seconds) then (getvy player) * upperTime else 0
            leftDist  = if (leftTime  <= seconds) then (getvx player) * leftTime  else 0
            rightDist = if (rightTime <= seconds) then (getvx player) * rightTime else 0

            player'   = if (downCol && (downTime <= seconds)) 
                      then (mvPlayer (0, downDist)) . (setvy (max 0 (getvy player))) $ player
                      else player
            player''  = if (upperCol && (upperTime <= seconds)) 
                      then (mvPlayer (0, upperDist)) . (setvy (min 0 (getvy player'))) $ player'
                      else player'
            player''' = if (leftCol && (leftTime <= seconds)) 
                      then (mvPlayer (leftDist, 0)) . (setvx (max 0 (getvx player''))) $ player''
                      else player''
            newPlayer = if (rightCol && (rightTime <= seconds)) 
                      then (mvPlayer (rightDist, 0)) . (setvx (min 0 (getvx player'''))) $ player'''
                      else player'''
            mvPlayer (x, y) p = (setx1 ((getx1 p) + x)) .
                                (setx2 ((getx2 p) + x)) .
                                (sety1 ((gety1 p) + y)) .
                                (sety2 ((gety2 p) + y)) $
                                p


nonZeroIntersection :: (Float, Float) -> (Float, Float) -> Bool
nonZeroIntersection (a, b) (c, d) =
    if  | (c >= a) && (c < b) -> True
        | (a >= c) && (a < d) -> True
        | otherwise -> False


objDownCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли нижняя сторона первого прямоугольника 
--верхней стороны второго
--(Bool, Float) - произойдёт ли коллизия, и если да, то через сколько секунд
objDownCollision (Object ax1 ax2 ay1 ay2 avx avy) (Object bx1 bx2 by1 by2 bvx bvy) =
    -- | 1) Если объекты уже стоят вплотную друг к другу по y, то нужно посмотреть, пересекаются ли они по x
     if | (abs dist) < eps -> if (nonZeroIntersection (ax1, ax2) (bx1, bx2)) then (True, 0) else (False, 0)
    -- | 2) Перейдём в систему отсчёта относительно второго объекта, т. е. пусть двигается только первый объект
    -- | Скорость первого объекта по y должна быть направлена вниз (отрицательна), иначе объекты отдаляются
        | vy' > -eps  -> (False, 0)
    -- | 3) Установлено, что первый объект движется вниз
    -- | Если он уже находится ниже второго объекта, то момент коллизии пройден
        | ay1 < by2  -> (False, 0)
    -- | 4) Установлено, что первый объект движется вниз, и коллизия может произойти
    -- | Тогда она произойдёт через time = dist / vy секунд и в том и только в том случае,
    -- | если через time секунд соответствующие стороны объектов будут пересекаться
        | nonZeroIntersection (ax1', ax2') (bx1, bx2) -> (True, time)
    -- | 5) Первый объект улетает куда-то в сторону, и коллизии не будет
        | otherwise  -> (False, 0)
        where
            dist = ay1 - by2
            vx'  = avx - bvx
            vy'  = avy - bvy
            time = abs (dist / vy')
            ax1' = ax1 + vx' * time
            ax2' = ax2 + vx' * time


objUpperCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли верхняя сторона первого прямоугольника нижней стороны второго
objUpperCollision a b = objDownCollision b a


objRightCollision :: Object -> Object -> (Bool, Float) --проверить, касается ли правая сторона первого прямоугольника левой стороны второго
--(Bool, Float) - произойдёт ли коллизия, и если да, то через сколько секунд
objRightCollision (Object ax1 ax2 ay1 ay2 avx avy) (Object bx1 bx2 by1 by2 bvx bvy) =
     if | (abs dist) < eps -> if (nonZeroIntersection (ay1, ay2) (by1, by2)) then (True, 0) else (False, 0) -- если вплотную
        | vx' < eps   -> (False, 0) -- отдаляются
        | ax2 > bx1  -> (False, 0) -- первый правее второго
        | nonZeroIntersection (ay1', ay2') (by1, by2) -> (True, time)
        | otherwise  -> (False, 0)
        where
            dist = bx1 - ax2
            vx'  = avx - bvx
            vy'  = avy - bvy
            time = abs (dist / vx')
            ay1' = ay1 + vy' * time
            ay2' = ay2 + vy' * time


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


-- | Определяет, стоит ли заданный игрок на поверхности <=> может ли заданный игрок совершить прыжок
canJump :: (GameState -> Player) -> GameState -> Bool
canJump getPlayer game =
    (downList /= []) && ((minimum valuesList) <= seconds)
        where
            player  = getPlayer game
            seconds = secsLeft game
            valuesList = map snd downList
            downList   = filter fst $ map (downCollision player) blockList -- [(True, Float)] или []
            blockList  = blocks game


-- | Пытается респавнить первого игрока, если время пришло
respawnPlayer1 :: GameState -> GameState
respawnPlayer1 game = game { player1 = newPlayer' }
    where
        player = player1 game
        newPlayer = if (not (alive player)) && ((timeToRespawn player) < eps)
                    then movePlayerToPoint (respawnPoint initPlayer1) initPlayer1
                    else player
        newPlayer' = newPlayer { invisible = False,
    							 blinkingTime = 5.0,
    							 sameStateTime = 0.25
    							}


-- | Пытается респавнить второго игрока, если время пришло
respawnPlayer2 :: GameState -> GameState
respawnPlayer2 game = game { player2 = newPlayer' }
    where
        player = player2 game
        newPlayer = if (not (alive player)) && ((timeToRespawn player) < eps)
                    then movePlayerToPoint (respawnPoint initPlayer2) initPlayer2
                    else player
        newPlayer' = newPlayer { invisible = False,
    							 blinkingTime = 5.0,
    							 sameStateTime = 0.25
    							}


-- | Передвигает игрока в рамках одного кадра с учётом гравитации
movePlayer :: Float -> Bool -> Player -> Player
movePlayer seconds canjump player = setx1 x1' $ setx2 x2' $ sety1 y1' $ sety2 y2' $ setvy vy' $ player
    where
        x1' = (getx1 player) + (getvx player) * seconds
        x2' = (getx2 player) + (getvx player) * seconds
        y1' = (gety1 player) + (getvy player) * seconds
        y2' = (gety2 player) + (getvy player) * seconds
        vy' = if (canjump) then (getvy player) else ((getvy player) - ge * seconds)


-- | Передвигает первого игрока в рамках одного кадра с учётом гравитации
movePlayer1 :: GameState -> GameState
movePlayer1 game = game { player1 = newPlayer }
    where
        seconds   = secsLeft game
        canjump   = canJump player1 game
        newPlayer = movePlayer seconds canjump (player1 game)


-- | Передвигает второго игрока в рамках одного кадра с учётом гравитации
movePlayer2 :: GameState -> GameState
movePlayer2 game = game { player2 = newPlayer }
    where
        seconds   = secsLeft game
        canjump   = canJump player2 game
        newPlayer = movePlayer seconds canjump (player2 game)


-- | Передвигает игрока к точке
movePlayerToPoint :: (Float, Float) -> Player -> Player
movePlayerToPoint (x, y) player = newPlayer
    where
        dx = (getx2 player) - (getx1 player)
        dy = (gety2 player) - (gety1 player)
        newPlayer = setx1 x $ setx2 (x + dx) $ sety1 y $ sety2 (y + dy) $ player


-- | Поворачивает игрока влево (см. handlePlayer1MovingKeys)
turnPlayerLeft :: Player -> Player
turnPlayerLeft player = player { turnedRight = False }


-- | Поворачивает игрока вправо (см. handlePlayer1MovingKeys)
turnPlayerRight :: Player -> Player
turnPlayerRight player = player { turnedRight = True }


-- | Обрабатывает выстрел первого игрока
handlePlayer1Shooting :: GameState -> GameState
handlePlayer1Shooting game = 
    if (member (Char 'q') (kbState game) && (time < eps)) then initBullet player1 newgame else game
      where 
        time = timeToReload.player1$game
        newgame = game{
          player1 = newPlayer
        }
        newPlayer = (player1 game) {
          timeToReload = 1.0 / rateOfFire
        }


-- | Обрабатывает выстрел второго игрока
handlePlayer2Shooting :: GameState -> GameState
handlePlayer2Shooting game = 
    if (member (SpecialKey KeyCtrlR) (kbState game) && (time < eps)) then initBullet player2 newgame else game
      where 
        time = timeToReload.player2$game
        newgame = game{
          player2 = newPlayer
        }
        newPlayer = (player2 game) {
          timeToReload = 1.0 / rateOfFire
        }

{-
-- | Производит выстрел от конкретного игрока
initBullet :: (GameState -> Player) -> GameState -> GameState
initBullet getPlayer game = undefined
-- Нужно реализовать
-- Добавить в список пуль новую с учётом положения, направления и скорости игрока
-}

-- | Производит выстрел от конкретного игрока
initBullet :: (GameState -> Player) -> GameState -> GameState
initBullet getPlayer  game = game {
        bullets1 = newbullet : (bullets1 game)
    }
    where
        newbullet = Bullet {
            bulletobj = bulObj,
            damage = bulletDamage,
            bulletColor = light green
            }
        player    = getPlayer game
        newObject = getObject player
        bulObj = setx1 newx1 $ setx2 newx2 $ sety1 newy1 $ sety2 newy2 $ setvx bSpeed $ setvy 0 $ newObject
        -- bulObj = setx1 ((getx1 newObject) + (getx2 newObject) / 2 + bulletSize) $ setx2 ((getx2 newObject) + (getx2 newObject) / 2 - bulletSize) 
        --   $ sety1 ((gety1 newObject)+ (gety2 newObject) / 2 + bulletSize)   $ sety2 ((gety2 newObject)+ (gety1 newObject) / 2 - bulletSize) $ 
        --   setvx bulletspeed $ newObject
        -- newx1 = ((getx1 newObject) + (getx2 newObject - getx1 newObject) /2)
        -- newx2 = ((getx2 newObject) - (getx2 newObject - getx1 newObject) /2)
        -- newy1 = ((gety1 newObject) + (gety2 newObject - gety1 newObject) /2)
        -- newy2 = ((gety2 newObject) - (gety2 newObject - gety1 newObject) /2)
        deltaDistance = if (turnedRight player) then 5 else (-5)
        bSpeed = if(turnedRight player) then  bulletspeed else (- bulletspeed)
        newx1 = ( (getx2 newObject + getx1 newObject) /2) - bulletSize + deltaDistance
        newx2 = ( (getx2 newObject + getx1 newObject) /2) + bulletSize + deltaDistance
        newy1 = ( (gety2 newObject + gety1 newObject) /2) - bulletSize 
        newy2 = ( (gety2 newObject + gety1 newObject) /2) + bulletSize 

      -- newPlayer = setx1 x $ setx2 (x + dx) $ sety1 y $ sety2 (y + dy) $ player


{-
-- | Передвигает пули
moveBullets :: GameState -> GameState
moveBullets game = undefined
-- Нужно реализовать
-- Обрабатывать коллизии с игроками не нужно, так как все пули, которые попали в игроков, исчезли на предыдущих этапах (см. handlePlayer1BulletCollisions)
-- Обработать коллизии с блоками (отражение, исчезновение - на ваш вкус)
-}

-- | Передвигает пули
-- что делает эта функция
--

moveBulletsHelper :: Float -> Bullet -> Bullet
moveBulletsHelper seconds bullet = setx1 x1' $ setx2 x2' $ sety1 y1' $ sety2 y2' $  bullet
    where
        x1' = (getx1 bullet) + (getvx bullet) * seconds
        x2' = (getx2 bullet) + (getvx bullet) * seconds
        y1' = (gety1 bullet) -- + (getvy bullet) * seconds
        y2' =  (gety2 bullet)-- + (getvy bullet) * seconds

moveBullets :: GameState -> GameState
moveBullets game = game {bullets1 = newBullets1}
    where
        seconds = secsLeft game
        newBullets1 = map (moveBulletsHelper seconds) (bullets1 game)

-- handleBuuletBlockCollisions :: GameState -> GameState
-- handleBuuletBlockCollisions game = game {
--     bullets1 = newBullets
--   }
--   where
--     seconds = secsLeft game
--     blockList = blocks game
--     player = player1 game 
--     newBulletsWithoutRightCols = filterBulletsColWithBlocks rightCollision blockList (bullets1 game)
--     newBulletsWithoutRightCols = filterBulletsColWithBlocks rightCollision blockList newBulletsWithoutRightCols (bullets1 game)

-- filterBulletsColWithBlocks :: 
{-

-- | 
handlePlayer2BlockCollisions :: GameState -> GameState
handlePlayer2BlockCollisions game = 
    game { player2 = newPlayer }
        where
            seconds = secsLeft game
            blockList = blocks game
            player = player2 game

            (downCol,  downTime)  = checkPlayerBlocksCollision downCollision  player blockList
            (upperCol, upperTime) = checkPlayerBlocksCollision upperCollision player blockList
            (leftCol,  leftTime)  = checkPlayerBlocksCollision leftCollision  player blockList
            (rightCol, rightTime) = checkPlayerBlocksCollision rightCollision player blockList

            newPlayer = updatePlayerWithBlockCollisions
                            [(downCol,  downTime),
                             (upperCol, upperTime),
                             (leftCol,  leftTime),
                             (rightCol, rightTime)
                             ] seconds player


-- | Проверяет, есть ли нижняя коллизия между заданным игроком и блоками, и если есть, то возвращает время до скорейшего столкновения
-- Первый аргумент - одна из функций: downCollision, upperCollision, leftCollision, rightCollision
checkPlayerBlocksCollision :: ( Player -> Block -> (Bool, Float)) -> Player -> [Block] -> (Bool, Float)
checkPlayerBlocksCollision fcol player blockList = (isCol, colTime)
    where
        values  = map snd $ filter fst $ map (fcol player) blockList -- [Float] или []
        isCol   = values /= []
        colTime = if isCol then minimum values else 0



-- | Вспомогательная функция для handlePlayer1BlockCollisions
updatePlayerWithBlockCollisions :: [(Bool, Float)] -> Float -> Player -> Player
updatePlayerWithBlockCollisions [(downCol,  downTime),
                                 (upperCol, upperTime),
                                 (leftCol,  leftTime),
                                 (rightCol, rightTime)] seconds player = 
    newPlayer
        where
            downDist  = if (downTime  <= seconds) then (getvy player) * downTime  else 0 -- на сколько сдвинуть
            upperDist = if (upperTime <= seconds) then (getvy player) * upperTime else 0
            leftDist  = if (leftTime  <= seconds) then (getvx player) * leftTime  else 0
            rightDist = if (rightTime <= seconds) then (getvx player) * rightTime else 0

            player'   = if (downCol && (downTime <= seconds)) 
                      then (mvPlayer (0, downDist)) . (setvy (max 0 (getvy player))) $ player
                      else player
            player''  = if (upperCol && (upperTime <= seconds)) 
                      then (mvPlayer (0, upperDist)) . (setvy (min 0 (getvy player'))) $ player'
                      else player'
            player''' = if (leftCol && (leftTime <= seconds)) 
                      then (mvPlayer (leftDist, 0)) . (setvx (max 0 (getvx player''))) $ player''
                      else player''
            newPlayer = if (rightCol && (rightTime <= seconds)) 
                      then (mvPlayer (rightDist, 0)) . (setvx (min 0 (getvx player'''))) $ player'''
                      else player'''
            mvPlayer (x, y) p = (setx1 ((getx1 p) + x)) .
                                (setx2 ((getx2 p) + x)) .
                                (sety1 ((gety1 p) + y)) .
                                (sety2 ((gety2 p) + y)) $
                                p

-}{-
-- | Нарисрвать текст
drawText :: Int -> Float -> Float -> String -> Picture
drawText k w h s = translate (-sw) sh (scale 30 30 (pictures (drawTextList k w h s)))
  where
    sw = fromIntegral screenWidth / 2
    sh = fromIntegral screenHeight / 2

-- | Составить список текста со смещением
drawTextList :: Int -> Float -> Float -> String -> [Picture]
drawTextList 0 _ _ _ = []
drawTextList k w h s = (drawTextFunc w h s) : (drawTextList (k-1) (w+0.02) (h+0.01) s) 

-- | Отрисоать одно слово
drawTextFunc :: Float -> Float -> String -> Picture
drawTextFunc w h s = translate w (h) (scale 0.01 0.01 (color black (text s)))
-}
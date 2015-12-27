module Main  where

import Config
import Graphics.UI.Fungen
import Attribute
import Paths_FunGEn (getDataFileName)
import Graphics.Rendering.OpenGL (GLdouble)

bmpList :: FilePictureList
bmpList = [("bordervert.bmp", Nothing),
        ("borderhor.bmp",  Nothing),
        ("field.bmp",      Nothing),
        ("headup.bmp",     Nothing),
        ("headleft.bmp",   Nothing),
        ("headdown.bmp",   Nothing),
        ("headright.bmp",  Nothing),
        ("tail.bmp",       Nothing),
        ("apple.bmp",      Nothing),
        ("start.bmp",      Nothing),
        ("finish.bmp",     Nothing)]

tileSize :: GLdouble
tileSize = 30.0

headPos, tail0Pos, tail1Pos :: Attribute.Position
headPos = (45.0,105.0)
tail0Pos = (45.0,75.0)
tail1Pos = (45.0,45.0)

initTailSize:: Int
initTailSize = 2

speed :: Speed
speed = 30.0

h,v,f :: NibblesTile
h = (1, True,  0.0, NoTileAttribute)
v = (0, True,  0.0, NoTileAttribute)
f = (2, False, 0.0, NoTileAttribute)

map :: Attribute.GameMap
map =  [[h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [v,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,v],
        [h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h]]

generateHead :: NibblesObject
generateHead = object "head" pic True headPos (0,speed) NoObjectAttribute
  where
        pic = Tex (tileSize, tileSize) 3

generateFood :: NibblesObject
generateFood = object "food" pic True (0,0) (0,0) NoObjectAttribute
  where
    pic = Tex (tileSize, tileSize) 8

generateMessage :: [NibblesObject]
generateMessage = [(object "start" picSt True (395,300) (0,0) NoObjectAttribute), (object "finish" picOv True (395,300) (0,0) NoObjectAttribute)]
  where
    picSt = Tex (300, 100) 9
    picOv = Tex (300, 100) 10

generateAsleepTail :: Int -> ObjectPicture -> [NibblesObject]
generateAsleepTail n pic
  | n == 2 = []
  | otherwise = (object ("tail"++(show n)) pic True (0,0) (0,0) (Tail 0)) : (generateAsleepTail (n - 1) pic)

generateTail :: [NibblesObject]
generateTail = (object "tail0" pic False tail0Pos (0,0) (Tail 0)):(object "tail1" pic False tail1Pos (0,0) (Tail 1)):(generateAsleepTail initTailSize pic)
  where
      pic = Tex (tileSize, tileSize) 7

turn :: Direction -> Modifiers -> Graphics.UI.Fungen.Position -> NibblesAction ()
turn dir _ _
  | dir == Attribute.Left = turn' (-speed, 0) 4
  | dir == Attribute.Right = turn' (speed, 0) 6
  | dir == Attribute.Up = turn' (0, speed) 3
  | dir == Attribute.Down = turn' (0, -speed) 5

turn' :: (Speed, Speed) -> Int -> NibblesAction()
turn' (s1, s2) ind = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture ind snakeHead
  setObjectSpeed (s1,s2) snakeHead

moveTail :: Attribute.Position -> NibblesAction()
moveTail headPosition = do
    -- GA StepTime Size Attribute.Position CurrentScore
    (GA timer size prevHeadPosition currentScore) <- getGameAttribute
    tails <- getObjectsFromGroup "tail"
    aliveTails <- getAliveTails tails []
    lastTail <- findLastTail aliveTails
    setObjectPosition headPosition lastTail
    setGameAttribute (GA timer size headPosition currentScore)
    changeTailsAttribute size aliveTails

getAliveTails :: [NibblesObject] -> [NibblesObject] -> NibblesAction [NibblesObject]
getAliveTails [] t = return t
getAliveTails (o:os) t = do
    sleeping <- getObjectAsleep o
    if sleeping
        then getAliveTails os t
        else getAliveTails os (o:t)
--
-- changeTailsAttribute :: Int -> [NibblesObject] -> NibblesAction ()
-- changeTailsAttribute _ [] = return ()
-- changeTailsAttribute tailSize (a:as) = do
--     setObjectAttribute (Tail (mod (n + 1) tailSize)) a
--     Tail n <- getObjectAttribute a
--     changeTailsAttribute tailSize as

changeTailsAttribute :: Int -> [NibblesObject] -> NibblesAction ()
changeTailsAttribute _ [] = return ()
changeTailsAttribute tailSize (a:as) = do
  Tail n <- getObjectAttribute a
  setObjectAttribute (Tail (mod (n + 1) tailSize)) a
  changeTailsAttribute tailSize as

findLastTail :: [NibblesObject] -> NibblesAction NibblesObject
findLastTail [] = error "the impossible has happened!"
findLastTail (t1:[]) = return t1
findLastTail (t1:t2:ts) = do
    (Tail na) <- getObjectAttribute t1
    (Tail nb) <- getObjectAttribute t2
    if (na > nb)
        then findLastTail (t1:ts)
        else findLastTail (t2:ts)

main = do
    let config = WindowConfig{
                         initialPosition=(100,100)
                       , initialSize=(780, 600)
                       , header = "Nibbles"
                       }
    let gameMap = tileMap Main.map tileSize tileSize
    let objects = [(objectGroup "messages" generateMessage),
                   (objectGroup "head"     [generateHead] ),
                   (objectGroup "food"     [generateFood] ),
                   (objectGroup "tail"     generateTail   )]
    let bindings = [(Char 'q', Press, \_ _ -> funExit)]
    bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("Nibbles/"++a); return (a', b)}) bmpList
    funInit (initialPosition config, initialSize config, header config)
                gameMap objects () () bindings (return()) Idle bmpList'

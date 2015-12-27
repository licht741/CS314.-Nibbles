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
headPos = (75.0,105.0)
tail0Pos = (75.0,75.0)
tail1Pos = (75.0,45.0)

initTailSize, defaultTimer:: Int
initTailSize = 2
defaultTimer = 10

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

gameCycle :: NibblesAction ()
gameCycle = do
  gameState <- getGameState
  gameCycle' gameState

startCycle :: StepTime -> NibblesAction ()
startCycle 0 = do
  (GA _ size prevHeadPosition currScore) <- getGameAttribute
  setGameState Level
  enableGameFlags
  snakeHead <- findObject "head" "head"
  setObjectAsleep False snakeHead
  setGameAttribute (GA 0 size prevHeadPosition currScore)
startCycle timer = do
  (GA _ size prevHeadPosition currScore) <- getGameAttribute
  setGameAttribute (GA (timer - 1) size prevHeadPosition currScore)
  
gameCycle' :: State -> NibblesAction ()
gameCycle' Start = do
  (GA timer _ _ _) <- getGameAttribute
  disableGameFlags
  level <- findObject "start" "messages"
  drawObject level
  startCycle timer
gameCycle' Level = do
  (GA timer _ _ _) <- getGameAttribute
  food <- findObject "food" "food"
  snakeHead <- findObject "head" "head"
  levelCycle timer food snakeHead
--showScore
gameCycle' Over = do
  disableMapDrawing
  gameover <- findObject "finish" "messages"
  drawObject gameover

levelCycle :: StepTime -> NibblesObject -> NibblesObject -> NibblesAction ()
levelCycle 0 food snakeHead = do
  (GA _ size prevHeadPosition currentScore) <- getGameAttribute
  newPosition <- createNewFoodPosition
  setObjectPosition newPosition food
  newFood <- findObject "food" "food"
  setObjectAsleep False newFood
  setGameAttribute (GA (-1) size prevHeadPosition currentScore)
  checkSnakeCollision snakeHead
  snakeHeadPosition <- getObjectPosition snakeHead
  moveTail snakeHeadPosition
levelCycle _ food snakeHead = do
  col <- objectsCollision snakeHead food
  if col
    then (do
	    (GA _ size prevHeadPosition currentScore) <- getGameAttribute
  	    snakeHeadPosition <- getObjectPosition snakeHead
  	    setGameAttribute (GA 0 (size + 1) snakeHeadPosition (currentScore + 1))
            addTail prevHeadPosition
            setObjectAsleep True food)
    else (do
            checkSnakeCollision snakeHead
            snakeHeadPosition <- getObjectPosition snakeHead               
            moveTail snakeHeadPosition)

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

generateAsleepTail :: Int -> Int -> ObjectPicture -> [NibblesObject]
generateAsleepTail n m pic
  | n > m = []
  | otherwise = (object ("tail"++(show n)) pic True (0,0) (0,0) (Tail 0)) : (generateAsleepTail (n + 1) m pic)

-- createTail = let picTail = Tex (tileSize,tileSize) 10
            --  in (object "tail0"  picTail False tail0Pos (0,0) (Tail 0)):
            --     (object "tail1"  picTail False tail1Pos (0,0) (Tail 1)):
            --     (createAsleepTails initTailSize (initTailSize + maxFood - 1) picTail)
generateTail :: [NibblesObject]
generateTail = (object "tail0" pic False tail0Pos (0,0) (Tail 0)):
                (object "tail1" pic False tail1Pos (0,0) (Tail 1)):
                (generateAsleepTail initTailSize (initTailSize + 99) pic)
  where
      pic = Tex (tileSize, tileSize) 7

moveTail :: Attribute.Position -> NibblesAction()
moveTail headPosition = do
    -- GA StepTime Size Attribute.Position CurrentScore
    (GA timer size _ currentScore) <- getGameAttribute
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

createNewFoodPosition :: NibblesAction (GLdouble,GLdouble)
createNewFoodPosition = do
    x <- randomInt (1,18)
    y <- randomInt (1,24)
    mapPositionOk <- checkMapPosition (x,y)
    tails <- getObjectsFromGroup "tail"
    tailPositionNotOk <- pointsObjectListCollision (toPixelCoord y) (toPixelCoord x) tileSize tileSize tails
    if (mapPositionOk && not tailPositionNotOk)
        then (return (toPixelCoord y,toPixelCoord x))
        else createNewFoodPosition
    where toPixelCoord a = (tileSize/2) + (fromIntegral a) * tileSize

checkMapPosition :: (Int,Int) -> NibblesAction Bool
checkMapPosition (x,y) = do
    mapTile <- getTileFromIndex (x,y)
    return (not (getTileBlocked mapTile))

checkSnakeCollision :: NibblesObject -> NibblesAction ()
checkSnakeCollision snakeHead = do
    headPos <- getObjectPosition snakeHead
    tile <- getTileFromWindowPosition headPos
    tails <- getObjectsFromGroup "tail"
    col <- objectListObjectCollision tails snakeHead
    if ( (getTileBlocked tile) || col)
        then (do setGameState Over
                 disableObjectsDrawing
                 disableObjectsMoving
                 setGameAttribute (GA 0 0 (0,0) 0))
        else return ()

getAsleepTail ::  [NibblesObject] ->  NibblesAction NibblesObject
getAsleepTail [] = error "the impossible has happened!"
getAsleepTail (o:os) = do
  sleeping <- getObjectAsleep o
  if sleeping
    then return o
    else getAsleepTail os

addTailNumber :: [NibblesObject] -> NibblesAction ()
addTailNumber [] = return ()
addTailNumber (a:as) = do
  (Tail n) <- getObjectAttribute a
  setObjectAttribute (Tail (n + 1)) a
  addTailNumber as

addTail :: (GLdouble,GLdouble) -> NibblesAction ()
addTail presentHeadPos = do
    tails <- getObjectsFromGroup "tail"
    aliveTails <- getAliveTails tails []
    asleepTail <-  getAsleepTail tails
    setObjectAsleep False asleepTail
    setObjectPosition presentHeadPos asleepTail
    setObjectAttribute (Tail 0) asleepTail
    addTailNumber aliveTails

turn :: Direction -> Modifiers -> Graphics.UI.Fungen.Position -> NibblesAction ()
turn dir _ _
    | dir == Attribute.Left = turn' (-speed, 0) 4
    | dir == Attribute.Right = turn' (speed, 0) 6
    | dir == Attribute.Up = turn' (0, speed) 3
    | dir == Attribute.Down = turn' (0, -speed) 5

turn' :: (Speed, Speed) -> Int -> NibblesAction ()
turn' (s1, s2) ind = do
    snakeHead <- findObject "head" "head"
    setObjectCurrentPicture ind snakeHead
    setObjectSpeed (s1,s2) snakeHead

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


    let bindings = [(Char 'q', Press, \_ _ -> funExit),
                    (SpecialKey KeyUp, Press, turn Up),
                    (SpecialKey KeyLeft, Press, turn Attribute.Left),
                    (SpecialKey KeyRight, Press, turn Attribute.Right),
                    (SpecialKey KeyDown, Press, turn Down)]

    bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("Nibbles/"++a); return (a', b)}) bmpList
    let wConf = (initialPosition config, initialSize config, header config)
    -- data NibblesProperties = GA StepTime Size Attribute.Position CurrentScore
    let gameAttribute = GA defaultTimer 2 headPos 0
    funInit wConf gameMap objects Start gameAttribute bindings gameCycle (Timer 150) bmpList'

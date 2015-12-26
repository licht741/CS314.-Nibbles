module Main  where

import Config
import Graphics.UI.Fungen
import Attribute
import Paths_FunGEn (getDataFileName)

bmpList :: FilePictureList
bmpList = [("bordervert.bmp", Nothing),
           ("borderhor.bmp",  Nothing),
	   ("field.bmp",      Nothing),
	   ("headup.bmp",     Nothing),
	   ("headleft.bmp",   Nothing),
	   ("headdown.bmp",   Nothing),
	   ("headrigth.bmp",  Nothing),
	   ("tail.bmp",       Nothing)]

tileSize :: Size
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

main = do
    let config = WindowConfig{
                         initialPosition=(100,100)
                       , initialSize=(780, 600)
                       , header = "Nibbles"
                       }
    let gameMap = tileMap Main.map tileSize tileSize
    let bindings = [(Char 'q', Press, \_ _ -> funExit)]
    bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("Nibbles/"++a); return (a', b)}) bmpList
    funInit (initialPosition config, initialSize config, header config)
                gameMap [] () () bindings (return()) Idle bmpList'

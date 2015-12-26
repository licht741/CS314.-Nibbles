module Main  where

import Config
import Graphics.UI.Fungen
import Attribute
import Paths_FunGEn (getDataFileName)

bmpList :: FilePictureList
bmpList = [("bordervert.bmp", Nothing),
           ("borderhor.bmp",  Nothing),
	   ("field.bmp",      Nothing)]

tileSize :: Size
tileSize = 30.0

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

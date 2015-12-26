module Main  where

import Config
import Graphics.UI.Fungen

main = do
    let config = WindowConfig{
                         initialPosition=(100,100)
                       , initialSize=(300, 300)
                       , header = "Nibbles"
                       }
    let gameMap = colorMap 0.0 0.0 0.0 250 250
    let bindings = [(Char 'q', Press, \_ _ -> funExit)]
    funInit (initialPosition config, initialSize config, header config)
                gameMap [] () () bindings (return()) Idle []

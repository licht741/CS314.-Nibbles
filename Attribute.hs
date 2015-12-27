module Attribute where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)


type StepTime = Int
type CurrentScore = Int
type Position = (GLdouble, GLdouble)
--type Size = GLdouble
-- type Size = GLdouble
type Size = Int
type Speed = GLdouble

data Direction = Left | Right | Up | Down deriving (Eq)

-- gameAttribute =     GA 	defaultTimer maxFood initTailSize    initPos 		 0
--data GameAttribute = GA       Int 		Int 	Int 	(GLdouble,GLdouble) Int
data NibblesProperties = GA StepTime Size Attribute.Position CurrentScore

data State = Start | Level | Over

data ObjectProperties = NoObjectAttribute | Tail Int
data TileAttribute = NoTileAttribute

type NibblesAction a = IOGame NibblesProperties ObjectProperties State TileAttribute a
type NibblesObject = GameObject ObjectProperties
type NibblesTile = Tile TileAttribute
type GameMap = TileMatrix TileAttribute

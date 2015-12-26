module Attribute where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)


type StepTime = Int
type CurrentScore = Int
type Position = (GLdouble, GLdouble)
type Size = GLdouble
type Speed = GLdouble

data Direction = Left | Right | Up | Down deriving (Eq)

data NibblesProperties = GA StepTime Size Attribute.Position CurrentScore

data State = GameEnabled | GameDisabled

data ObjectProperties = NoObjectAttribute | Tail Int
data TileAttribute = NoTileAttribute

type NibblesAction a = IOGame NibblesProperties ObjectProperties State TileAttribute a
type NibblesObject = GameObject ObjectProperties
type NibblesTile = Tile TileAttribute
type GameMap = TileMatrix TileAttribute

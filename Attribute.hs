module Attribute where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)


type StepTime = Int
type CurrentScore = Int
type Position = (GLdouble, GLdouble)
type Size = GLdouble
type Speed = GLdouble

data NibblesProperties = GA StepTime Size Position CurrentScore

data State = GameEnabled | GameDisabled

data ObjectProperties = NoObjectAttribute | Tail Int
data TileAttribute = NoTileAttribute

type NibblesActionIO a = IOGame NibblesProperties ObjectProperties State TileAttribute a
type NibblesObject = GameObject ObjectProperties
type NibblesTile = Tile TileAttribute
type GameMap = TileMatrix TileAttribute

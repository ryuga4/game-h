{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Mouse as Mouse
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time


data Action = Idle
            | ChangePosition (V2 Double)
            | ChangeDirection Direction
            | Move Double
            

data Model = Model
  { cursorPos :: V2 Double
  , direction :: Direction
  , snake :: Snake
  , snakeLength :: SnakeSize
  }

data Direction = DLeft | DRight | DUp | DDown
type Snake = [V2 Double]
type SnakeSize = Int



moveSnake :: Direction -> SnakeSize -> Snake -> Snake
moveSnake _ _ [] = []
moveSnake direction n snake@(V2 x y : _) = take n $ newHead direction : snake
  where newHead DLeft = V2 (x-10) y
        newHead DRight = V2 (x+10) y
        newHead DUp = V2 x (y-10)
        newHead DDown = V2 x (y+10)


initial :: (Model, Cmd SDLEngine Action)
initial = (model, Cmd.none)
  where model = Model
          { cursorPos = V2 0 0
          , direction = DLeft
          , snake = [V2 405 405]
          , snakeLength = 20
          }

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model (ChangePosition pos) = (model {cursorPos = pos}, Cmd.none)
update model (ChangeDirection direction) = (model {direction = direction}, Cmd.none)
update (model@Model {..}) (Move _) = (model {snake = newSnake}, Cmd.none)
  where newSnake = moveSnake direction snakeLength snake


subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Mouse.moves (\(V2 x y) -> ChangePosition $ V2 (fromIntegral x) (fromIntegral y))
  , Keyboard.downs (\case
                         Keyboard.UpKey -> ChangeDirection DUp
                         Keyboard.DownKey -> ChangeDirection DDown
                         Keyboard.LeftKey -> ChangeDirection DLeft
                         Keyboard.RightKey -> ChangeDirection DRight
                         _ -> Idle
                     )
  , Time.fps 20 Move
  ]
view :: Model -> Graphics SDLEngine
view Model { .. } = Graphics2D $ collage s
  where s = map (\i -> move i $ filled (rgb 0 1 1) $ square 10) snake

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowDimensions = V2 800 800
    , SDL.windowIsResizable = False
    }
  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

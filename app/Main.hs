{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL
import           Helm.Graphics2D

import qualified Helm.Cmd        as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard   as Keyboard
import qualified Helm.Mouse      as Mouse
import qualified Helm.Sub        as Sub
import qualified Helm.Time       as Time
import           System.Random

data Action = Idle
            | ChangePosition (V2 Double)
            | ChangeDirection Direction
            | Move Double
            | SpawnApple Double


data Model = Model
  { cursorPos   :: V2 Double
  , direction   :: Direction
  , snake       :: Snake
  , snakeLength :: SnakeSize
  , apples      :: [V2 Int]
  , randGen     :: StdGen
  }

data Direction = DLeft | DRight | DUp | DDown
  deriving (Eq)

oposite :: Direction -> Direction
oposite DLeft  = DRight
oposite DRight = DLeft
oposite DUp    = DDown
oposite DDown  = DUp

type Snake = [V2 Int]
type SnakeSize = Int


moveSnake :: Model -> Model
moveSnake model@Model {snake=[]} = model
moveSnake model@Model { .. } = model { snake = newSnake
                                     , snakeLength = newSnakeLength
                                     , apples = newApples
                                     }
  where
        newHead DLeft  = head snake - V2 10 0
        newHead DRight = head snake + V2 10 0
        newHead DUp    = head snake - V2 0 10
        newHead DDown  = head snake + V2 0 10
        appleHit = head snake `elem` apples
        newSnake = take snakeLength $ newHead direction : snake
        (newApples, newSnakeLength)  | appleHit = (filter (head snake /=) apples, snakeLength + 1)
                                     | otherwise = (apples, snakeLength)


initial :: StdGen -> (Model, Cmd SDLEngine Action)
initial gen = (model, Cmd.none)
  where model = Model
          { cursorPos = V2 0 0
          , direction = DLeft
          , snake = [V2 405 405]
          , snakeLength = 20
          , apples = []
          , randGen = gen
          }

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model (ChangePosition pos) = (model {cursorPos = pos}, Cmd.none)
update model@Model{..} (ChangeDirection newDirection) =
  if direction /= newDirection
  then (model {direction = newDirection}, Cmd.none)
  else (model, Cmd.none)
update model (Move _) = (moveSnake model, Cmd.none)
update model (SpawnApple _) = (model { apples = newApple : apples model
                                     , randGen = gen2
                                     }, Cmd.none)
  where (rand1, gen1) = next $ randGen model
        (rand2, gen2) = next gen1
        x = rand1 `mod` 60 * 10 + 5
        y = rand2 `mod` 60 * 10 + 5
        newApple = V2 x y

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
  , Time.every (Time.second * 2) SpawnApple
  ]

view :: Model -> Graphics SDLEngine
view Model { .. } = Graphics2D $ collage $ a ++ s
  where s = map (\i -> move (fmap fromIntegral i) $ filled (rgb 0 1 0) $ square 10) snake
        a = map (\i -> move (fmap fromIntegral i) $ filled (rgb 1 0 0) $ circle 5) apples

main :: IO ()
main = do
  rand <- getStdGen
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowDimensions = V2 600 600
    , SDL.windowIsResizable = False
    }
    
  run engine GameConfig
    { initialFn       = initial rand
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

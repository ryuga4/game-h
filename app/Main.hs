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
            | ToggleState
data PlayerState = Playing
                 | Pause
                 | Dead
                 deriving (Eq)

data Model = Model
  { cursorPos     :: V2 Double
  , direction     :: Direction
  , nextDirection :: Direction
  , snake         :: Snake
  , snakeLength   :: SnakeSize
  , apples        :: [V2 Int]
  , randGen       :: StdGen
  , playerState   :: PlayerState
  }


data Direction = DLeft | DRight | DUp | DDown
  deriving (Eq)

type Snake = [V2 Int]
type SnakeSize = Int

initial :: StdGen -> (Model, Cmd SDLEngine Action)
initial gen = (model, Cmd.none)
  where model = Model
          { cursorPos = V2 0 0
          , direction = DLeft
          , nextDirection = DLeft
          , snake = [V2 405 405]
          , snakeLength = 20
          , apples = []
          , randGen = gen
          , playerState = Playing
          }


oposite :: Direction -> Direction
oposite DLeft  = DRight
oposite DRight = DLeft
oposite DUp    = DDown
oposite DDown  = DUp

toggleState :: PlayerState -> PlayerState
toggleState Playing = Pause
toggleState Pause   = Playing
toggleState Dead    = Playing

moveSnake :: Model -> Model
moveSnake model@Model {snake=[]} = model
moveSnake model@Model { .. } = model { snake = newSnake
                                     , snakeLength = newSnakeLength
                                     , apples = newApples
                                     , playerState = newPlayerState
                                     , direction = nextDirection
                                     }
  where
        directionVec DLeft  = V2 (-10) 0
        directionVec DRight = V2 10 0
        directionVec DUp    = V2 0 (-10)
        directionVec DDown  = V2 0 10
        newHead = (`mod` 600) <$> head snake + directionVec nextDirection
        appleHit = head snake `elem` apples
        snakeHit = head snake `elem` tail snake
        newPlayerState | snakeHit  = Dead
                       | otherwise = playerState
        newSnake = take snakeLength $ newHead : snake
        (newApples, newSnakeLength)  | appleHit = (filter (head snake /=) apples, snakeLength + 1)
                                     | otherwise = (apples, snakeLength)



update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model (ChangePosition pos) = (model {cursorPos = pos}, Cmd.none)
update model@Model{..} (ChangeDirection newDirection) =
  if direction /= oposite newDirection
  then (model {nextDirection = newDirection}, Cmd.none)
  else (model, Cmd.none)
update model (Move _) | playerState model == Playing = (moveSnake model, Cmd.none)
                      | otherwise = (model, Cmd.none)
update model (SpawnApple _) | playerState model == Playing =
                              (model { apples = newApple : apples model
                                     , randGen = gen2
                                     }, Cmd.none)
                            | otherwise = (model, Cmd.none)
  where (rand1, gen1) = next $ randGen model
        (rand2, gen2) = next gen1
        x = rand1 `mod` 60 * 10 + 5
        y = rand2 `mod` 60 * 10 + 5
        newApple = V2 x y
update model ToggleState | playerState model == Dead = initial $ randGen model
                         | otherwise = (model {playerState = toggleState $ playerState model}, Cmd.none)


subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Mouse.moves (\(V2 x y) -> ChangePosition $ V2 (fromIntegral x) (fromIntegral y))
  , Keyboard.downs (\case
                         Keyboard.UpKey -> ChangeDirection DUp
                         Keyboard.DownKey -> ChangeDirection DDown
                         Keyboard.LeftKey -> ChangeDirection DLeft
                         Keyboard.RightKey -> ChangeDirection DRight
                         Keyboard.SpaceKey -> ToggleState
                         _ -> Idle
                     )
  , Time.fps 20 Move
  , Time.every (Time.second * 2) SpawnApple
  ]

view :: Model -> Graphics SDLEngine
view Model { .. } | playerState == Dead = Graphics2D $ collage
                                          [move (V2 400 400)
                                           $ filled (rgb 1 1 1)
                                           $ square 100]

                  | otherwise = Graphics2D $ collage $ a ++ s
  where s = snake >>= \i -> map (move (fmap fromIntegral i))
                                  [ filled (rgb 0 0.5 0) $ square 10
                                  , outlined (solid (rgb 0.6 0.8 0)) $ square 10
                                  ]
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

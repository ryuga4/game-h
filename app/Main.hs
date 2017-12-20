{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL
import           Helm.Graphics2D

import           Data.Maybe
import           Debug.Trace
import qualified Helm.Cmd        as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard   as Keyboard
import qualified Helm.Mouse      as Mouse
import qualified Helm.Sub        as Sub
import qualified Helm.Time       as Time
import           Lib             as Lib
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
  { cursorPos      :: V2 Double
  , directionQueue :: Queue Direction
  , direction      :: Direction
  , nextDirection  :: Direction
  , snake          :: Snake
  , snakeLength    :: SnakeSize
  , apples         :: [V2 Int]
  , randGen        :: StdGen
  , playerState    :: PlayerState
  }


data Direction = DLeft | DRight | DUp | DDown
  deriving (Eq, Show)

type Snake = [V2 Int]
type SnakeSize = Int

windowSize :: Int
windowSize = 600
framesNumber :: Int
framesNumber = 40
fps :: Int
fps = 5
segSize :: Int
segSize = windowSize `div` framesNumber
appleSpawnTime :: Double
appleSpawnTime = 3
pointsOnApple :: Int
pointsOnApple = 1







initial :: StdGen -> (Model, Cmd SDLEngine Action)
initial gen = (model, Cmd.none)
  where model = Model
          { cursorPos = V2 0 0
          , directionQueue = Queue [] []
          , direction = DLeft
          , nextDirection = DLeft
          , snake = [fmap (const (windowSize `div` 2 + segSize `div` 2)) $ V2 0 0]
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
moveSnake Model { .. } | trace (show directionQueue) False = undefined
moveSnake model@Model {snake=[]} = model
moveSnake model@Model { .. } = model { snake = newSnake
                                     , snakeLength = newSnakeLength
                                     , apples = newApples
                                     , playerState = newPlayerState
                                     , direction = newDirection
                                     , directionQueue = pop newDirectionQueue
                                     }
  where
        newDirectionQueue = popWhile (== oposite direction) directionQueue
        newDirection = fromMaybe direction $ front newDirectionQueue
        
        directionVec DLeft  = V2 (-segSize) 0
        directionVec DRight = V2 segSize 0
        directionVec DUp    = V2 0 (-segSize)
        directionVec DDown  = V2 0 segSize
        newHead = (`mod` windowSize) <$> head snake + directionVec newDirection

        appleHit = head snake `elem` apples
        snakeHit = head snake `elem` tail snake

        newPlayerState | snakeHit  = Dead
                       | otherwise = playerState

        newSnake = take snakeLength $ newHead : snake
        (newApples, newSnakeLength)  | appleHit = (filter (head snake /=) apples, snakeLength + pointsOnApple)
                                     | otherwise = (apples, snakeLength)
update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model (ChangePosition pos) = (model {cursorPos = pos}, Cmd.none)
update model@Model{..} (ChangeDirection newDirection) = (model {directionQueue = newQueue}, Cmd.none)
  where newQueue = push newDirection directionQueue
update model (Move _) | playerState model == Playing = (moveSnake model, Cmd.none)
                      | otherwise = (model, Cmd.none)
update model (SpawnApple _) | playerState model == Playing =
                              (model { apples = newApple : apples model
                                     , randGen = newRandGen
                                     }, Cmd.none)
                            | otherwise = (model, Cmd.none)
  where newApplePos gen = let
          (rand1, gen1) = next gen
          (rand2, gen2) = next gen1
          x = (mod rand1 framesNumber) * segSize + segSize `div` 2
          y = (mod rand2 framesNumber) * segSize + segSize `div` 2
          in
          if elem (V2 x y) $ snake model ++ apples model
            then newApplePos gen2
            else (V2 x y, gen2)
        (newApple, newRandGen) = newApplePos $ randGen model
        
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
  , Time.fps fps Move
  , Time.every (Time.second * appleSpawnTime) SpawnApple
  ]

view :: Model -> Graphics SDLEngine
view Model { .. } | playerState == Dead = Graphics2D $ collage
                                          [move (V2 400 400)
                                           $ filled (rgb 1 1 1)
                                           $ square 100]

                  | otherwise = Graphics2D $ collage $ a ++ s
  where s = snake >>= \i -> map (move (fmap fromIntegral i))
                                  [ filled (rgb 0 0.5 0) $ square $ fromIntegral segSize
                                  , outlined (solid (rgb 0.6 0.8 0)) $ square $ fromIntegral segSize
                                  ]
        a = map (\i -> move (fmap fromIntegral i) $ filled (rgb 1 0 0) $ circle $ fromIntegral segSize / 2) apples

main :: IO ()
main = do
  rand <- getStdGen
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowDimensions = V2 windowSize windowSize
    , SDL.windowIsResizable = False
    }

  run engine GameConfig
    { initialFn       = initial rand
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

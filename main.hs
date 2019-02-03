-- This sets up the state of the
-- program starts the main loop

-- Goal #1 - Create a map that the user can walk around
-- [X] Create and render a map
-- [X] Render the user at the starting location
-- [X] Update the user's location when the input changes
import Control.Monad.State.Lazy

data GameState = GameState {
    userLocation :: (Int, Int),
    roomDimensions :: (Int, Int)
} deriving (Show)

data UserAction = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Show)

-- Display a grid with the given dimensions
renderGrid :: GameState -> String
renderGrid gs =
    let (width, height) = roomDimensions gs
    in unlines $ replicate height $ replicate width '.'

-- Display a player on the screen
renderPlayerOnGrid :: GameState -> String -> String
renderPlayerOnGrid gs grid =
    let (width, height) = roomDimensions gs
        (x, y) = userLocation gs
        index = (width + 1) * y + x
    in replace grid '*' index
    where
        replace :: String -> Char -> Int -> String
        replace grid char index = 
            let (x,_:xs) = splitAt index grid
            in x ++ [char] ++ xs

-- Given an input Char, convert it into a UserAction
getUserAction :: Char -> Maybe UserAction
getUserAction c =
    case c of
        'h' -> Just MoveLeft
        'j' -> Just MoveDown
        'k' -> Just MoveUp
        'l' -> Just MoveRight
        _ -> Nothing
        
-- Given a UserAction, update the state
updateStateWithAction :: UserAction -> State GameState Bool
updateStateWithAction action = do
    gs <- get
    let (x, y) = userLocation gs
    let (width, height) = roomDimensions gs
    case action of
        MoveUp | y > 0 -> put $ gs { userLocation=(x, y-1) }
        MoveDown | y < (height-1) -> put $ gs { userLocation=(x, y+1) }
        MoveRight | x < (width-1) -> put $ gs { userLocation=(x+1, y) }
        MoveLeft | x > 0 -> put $ gs { userLocation=(x-1, y) }
        _ -> put gs
    return True


main = do
    state <- initialize
    loop state
    where
        initialize :: IO GameState
        initialize = do
            putStrLn "initializing"
            return GameState {
                userLocation=(5,5),
                roomDimensions=(10,10)
            }
        loop :: GameState -> IO ()
        loop gs = do
            -- -- Update the State with the input
            (x:_) <- getLine
            let mAction = getUserAction x
            let (continue, newState) = case mAction of
                                        Just action -> (runState (updateStateWithAction action) gs)
                                        Nothing -> (True, gs)
            -- Render the screen based on the State
            render newState
            if continue then loop newState else return ()
        render :: GameState -> IO ()
        render gs = do
            let grid = renderPlayerOnGrid gs (renderGrid gs)
            putStr grid
    
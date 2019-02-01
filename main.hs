-- This sets up the state of the
-- program starts the main loop

-- Goal #1 - Create a map that the user can walk around
-- [X] Create and render a map
-- [] Render the user at the starting location
-- [] Update the user's location when the input changes

data GameState = GameState {
    userLocation :: (Int, Int),
    roomDimensions :: (Int, Int)
} deriving (Show)

-- Display a grid with the given dimensions
renderGrid :: Int -> Int -> String
renderGrid width height = 
    unlines $ replicate height $ replicate width '.'

main = do
    state <- initialize
    render state
    where
        initialize :: IO GameState
        initialize = do
            return GameState {
                userLocation=(0,0),
                roomDimensions=(10,10)
            }
        render :: GameState -> IO ()
        render gs = do
            let (width, height) = roomDimensions gs
            putStr $ renderGrid width height
    
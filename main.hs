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
        replace grid char index = 
            let (x,_:xs) = splitAt index grid
            in x ++ [char] ++ xs

main = do
    state <- initialize
    render state
    where
        initialize :: IO GameState
        initialize = do
            return GameState {
                userLocation=(5,5),
                roomDimensions=(10,10)
            }
        render :: GameState -> IO ()
        render gs = do
            let grid = renderPlayerOnGrid gs (renderGrid gs)
            putStr grid
    
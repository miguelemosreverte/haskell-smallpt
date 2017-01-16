
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

{-
 , EasyTriangle (Vec(0,0,50), Vec(0,60,100), Vec (60,0, 50),
  Vec (73, 16.5, 78),Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75), Diff)   -- Blue Triangle
  -}

-- Loads lines from a text file into a list.
getLines :: FilePath -> IO [String]
getLines path = do contents <- readFile path
                   return (lines contents)
parseLine :: String -> (Float,Float,Float)
parseLine str = case words str of
    (a : b : c : _) ->  ((read a ::Float), (read b::Float), (read c::Float))
    _ ->  error "Bad line format"

main :: IO ()
main = do
      putStrLn "--- testing ---"

      vertex_list <-getLines "vertices.txt"
      let parsed_vertex_list = map parseLine vertex_list
      putStrLn (show parsed_vertex_list)

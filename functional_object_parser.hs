import qualified Data.Maybe
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

parseLineVertices :: String -> Maybe(Float,Float,Float)
parseLineVertices str = case words str of
    (a : b : c : d : e : f) ->  Just((read a ::Float), (read b::Float), (read c::Float))
    _ -> Nothing


parseLineTriangles :: String -> Maybe(Float,Float,Float)
parseLineTriangles str = case words str of
    ("3" : b : c : d : _) -> Just((read b ::Float), (read c::Float), (read d::Float))
    _ -> Nothing



main :: IO ()
main = do
      putStrLn "--- testing ---"

      vertex_list <-getLines "decimated_standford_bunny.ply"
      let parsed_vertex_list = Data.Maybe.catMaybes $ map parseLineVertices vertex_list
      let parsed_triangles_list = Data.Maybe.catMaybes $ map parseLineTriangles vertex_list
      putStrLn (show parsed_triangles_list)

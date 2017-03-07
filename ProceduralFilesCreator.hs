
module ProceduralFilesCreator where
import qualified Numeric
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

parseLineVertices :: String -> Maybe(Double,Double,Double)
parseLineVertices str = case words str of
    (a : b : c : d : e : f) ->  Just((read a ::Double), (read b::Double), (read c::Double))
    _ -> Nothing



parseLineTriangles :: String -> Maybe(Int,Int,Int)
parseLineTriangles str = case words str of
    ("3" : b : c : d : _) -> Just((read b ::Int), (read c::Int), (read d::Int))
    _ -> Nothing


prepareOutputVertices :: (Double,Double,Double) -> String
prepareOutputVertices (a,b,c) = show a ++ " " ++ show b ++ " " ++ show c
prepareOutputTriangles :: (Int,Int,Int) -> String
prepareOutputTriangles (a,b,c) = show a ++ " " ++ show b ++ " " ++ show c

createTrianglesAndVerticesFiles :: IO ()
createTrianglesAndVerticesFiles = do
      vertex_list <-getLines "decimated_standford_bunny.ply"
      let parsed_vertex_list = Data.Maybe.catMaybes $ map parseLineVertices vertex_list
      let parsed_triangles_list = Data.Maybe.catMaybes $ map parseLineTriangles vertex_list

      writeFile "vertices.txt"  (unlines (map prepareOutputVertices parsed_vertex_list))
      writeFile "triangles.txt"  (unlines (map prepareOutputTriangles parsed_triangles_list))



module Parser where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import ProceduralFilesCreator
{-
 , EasyTriangle (Vec(0,0,50), Vec(0,60,100), Vec (60,0, 50),
  Vec (73, 16.5, 78),Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75), Diff)   -- Blue Triangle
  -}

-- Loads lines from a text file into a list.
{-
getLines :: FilePath -> IO [String]
getLines path = do contents <- readFile path
                   return (lines contents)
-}
parseVerticesLine :: String -> (Double,Double,Double)
parseVerticesLine str = case words str of
    (a : b : c : _) ->  ((read a ::Double), (read b::Double), (read c::Double))
    _ ->  error "Bad line format"
--
parseTrianglesLine :: String -> (Int,Int,Int)
parseTrianglesLine str = case words str of
    (a : b : c : _) ->  ((read a ::Int), (read b::Int), (read c::Int))
    _ ->  error "Bad line format"

addCoordinatesTriangleConstructor :: String -> String
addCoordinatesTriangleConstructor coordinates  = "EasyTriangle " ++ coordinates
  ++ ",Vec (0,0,0),Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75), Diff)"

--
addVecPefix:: (Double,Double,Double) -> String
addVecPefix vec = "Vec " ++ show(vec)
--
coordinatesToString:: ((Double,Double,Double),(Double,Double,Double),(Double,Double,Double)) -> String
coordinatesToString (a,b,c)  = (addVecPefix a) ++ "," ++ (addVecPefix b) ++ "," ++ (addVecPefix c)

getVertices:: (Int,Int,Int) -> [(Double,Double,Double)] -> ((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))
getVertices (v0,v1,v2) vertex_list = ((vertex_list !! v0),(vertex_list !! v1),(vertex_list !! v2))
  {--
constructTriangleFromItsVertexIndexes::(Int,Int,Int) -> [(Double,Double,Double)]  ->  String
constructTriangleFromItsVertexIndexes verticesIndices parsed_vertex_list =
    addCoordinatesTriangleConstructor $ coordinatesToString $ (getVertices verticesIndices parsed_vertex_list)
  --}

constructTriangleFromItsVertexIndexes::(Int,Int,Int) -> [(Double,Double,Double)]  ->  ((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))
constructTriangleFromItsVertexIndexes verticesIndices parsed_vertex_list =
    getVertices verticesIndices parsed_vertex_list

getTrianglesConstructorsFromData:: FilePath -> FilePath -> IO[((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))]
getTrianglesConstructorsFromData vertices triangles =
  do
    pepe
    vertex_list <-getLines "vertices.txt"
    triangles_list <-getLines "triangles.txt"
    let parsed_vertex_list = map parseVerticesLine vertex_list
    let parsed_triangles_list = map parseTrianglesLine triangles_list
    let triangles_constructors_list = map (\x -> constructTriangleFromItsVertexIndexes (x) parsed_vertex_list) parsed_triangles_list
    return triangles_constructors_list

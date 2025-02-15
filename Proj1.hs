import Data.Array qualified
import Data.Bits qualified
import Data.List qualified

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

type AdjMatrix = Data.Array.Array (Int, Int) (Maybe Distance)

-- First, get a list with all the cities (with repeated elements). Then, sort it and group it, thus producing a list like this: [["0", "0"], ["1"], ["2", "2", "2"]]
-- Finally, get the head of all of the sublists, which outputs ["0", "1", "2"].
cities :: RoadMap -> [City]
cities [] = []
cities rm = map head (Data.List.group (Data.List.sort [city | (city1, city2, _) <- rm, city <- [city1, city2]]))

-- Filter all the tuples where the 2 cities are connected, if they exist return True, False otherwise.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm city1 city2 =
  (city1 /= city2)
    && ( case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) rm of
           [] -> False
           _ : _ -> True
       )

-- Very similar to areAdjacent, but return the distance (or Nothing if they are not adjacent).
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm city1 city2 =
  case filter (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) rm of
    [] -> Nothing
    ((_, _, d) : _) -> Just d

-- Iterate over every connection where one city matches the desired city and the other one is different. Then, extract the city and the distance.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent rm city =
  [ (if c1 == city then c2 else c1, d)
    | (c1, c2, d) <- rm,
      c1 == city || c2 == city, -- One of the cities matches the desired one.
      c1 /= city || c2 /= city -- One of the cities does not match the desired one (to prevent connections across the same city).
  ]

-- Total length of a path. Start by getting the distance between the first 2 cities, and then keep adding the distance to the next city.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Nothing
pathDistance _ [city] = Just 0
pathDistance rm (city1 : city2 : rest) =
  case distance rm city1 city2 of
    Nothing -> Nothing
    Just d -> case pathDistance rm (city2 : rest) of
      Nothing -> Nothing
      Just totalDist -> Just (d + totalDist)

-- Auxiliary function that returns the maximum distance of a list of tuples (City, Distance).
maxLength :: [(City, Distance)] -> Distance
maxLength [] = 0
maxLength ((_, l) : rest) = max l (maxLength rest)

-- Gets the cities with the most connections.
rome :: RoadMap -> [City]
rome rm = map fst (filter (\(c, l) -> l == max) cities)
  where
    cities = map (\x -> (head x, length x)) (Data.List.group (Data.List.sort [city | (city1, city2, _) <- rm, city <- [city1, city2]])) -- number of connections from a city
    max = maxLength cities -- maximum number of connections.

-- DFS function that takes the roadmap, the starting city and the list of visited cities, and returns the reachable cities.
-- If the city has already been visited, ignore it. Otherwise, mark it as visited and iterate over every adjacent city, calling dfs on each one, saving the result on an accumulator.
-- Takes a roadmap, city and array of visited cities (initially empty), and returns an array of visited cities.
dfs :: RoadMap -> City -> [City] -> [City]
dfs rm city visited
  | city `elem` visited = visited
  | otherwise = foldl (\acc (neighbor, _) -> dfs rm neighbor acc) (city : visited) (adjacent rm city)

-- An empty graph is connected by default. Otherwise, run DFS and check if all cities are reachable from one city, which can be done by comparing the length of all cities with the length of the reachable cities.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected rm
  | null allCities = True
  | otherwise = length allCities == length (dfs rm (head allCities) [])
  where
    allCities = cities rm

-- Auxialiary function that helps the shortespath function to find the shortest path between two cities.
-- The first argument is the roadmap, the second is the starting city, the third is the ending city
-- Returns a MAYBE tuple with the path and the distance.
dijkstras :: RoadMap -> City -> City -> Maybe (Path, Distance)
dijkstras roadMap start end = dijkstraHelper [(start, 0)] [] [(start, [start], 0)]
  where
    -- This auxiliary function does the heavy lifting of the dijkstras function.
    -- The first argument is the priority queue, the second is the list of visited cities, the third is the list of paths.
    dijkstraHelper :: [(City, Distance)] -> [(City, Distance)] -> [(City, Path, Distance)] -> Maybe (Path, Distance) -- queue, visited, paths
    dijkstraHelper [] _ _ = Nothing
    dijkstraHelper ((currentCity, currentDistance) : queue) visited paths
      | currentCity == end = case lookup currentCity [(c, (p, d)) | (c, p, d) <- paths] of
          Just (p, d) -> Just (reverse p, d)
          Nothing -> Just ([currentCity], currentDistance)
      | otherwise = dijkstraHelper newQueue newVisited newPaths
      where
        currentNeighbors = adjacent roadMap currentCity
        newVisited = (currentCity, currentDistance) : visited
        (newQueue, newPaths) = foldr update (queue, paths) currentNeighbors
        -- The update function is used to update the queue and the paths list. It also checks if the new distance is smaller than the old one.
        update (neighbor, dist) (q, pths) =
          let newDist = currentDistance + dist
              oldDist = case lookup neighbor (q ++ visited) of
                Nothing -> maxBound
                Just d -> d
           in if newDist < oldDist
                then ((neighbor, newDist) : filter ((/= neighbor) . fst) q, (neighbor, currentCity : p, newDist) : filter (\(c, _, _) -> c /= neighbor) pths)
                else (q, pths)
          where
            p = case lookup currentCity [(c, (p, d)) | (c, p, d) <- pths] of
              Nothing -> []
              Just (p', _) -> p'

-- The dfsPrune function is used to find all shortest paths between two cities that have the same distance as the shortest one.
-- The first argument is the roadmap, the second is the starting city, the third is the ending city, the fourth is the shortest path' distance already found by dijkstras.
dfsPrune :: RoadMap -> City -> City -> Distance -> [Path]
dfsPrune roadMap start end maxDist = dfsHelper start [start] 0
  where
    dfsHelper :: City -> Path -> Distance -> [Path]
    dfsHelper current path currentDist
      | current == end && currentDist == maxDist = [reverse path]
      | currentDist > maxDist = []
      | otherwise = concatMap (\(neighbor, dist) -> dfsHelper neighbor (neighbor : path) (currentDist + dist)) (adjacent roadMap current)

-- The shortestPath function is the mains function of the shortestPath module.
-- Firstly, it calls the dijkstras function to get the shortest path between two cities.
-- Then, it calls the dfsPrune function to get all the shortest paths between two cities.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end = case dijkstras roadMap start end of
  Nothing -> []
  Just (shortestPath, shortestDist) -> dfsPrune roadMap start end shortestDist

-- TSP types and algorithm based of the book "Algorithms: A Functional Programming Approach" by Fethi Rabhi and Guy Lapalme
-- A set is used to represent integers, where each bit in the set corresponds to the presence or absence of an element in the set.
-- It allows for efficient set operations using bitwise arithmetic.
type Set = Int

-- Creates an empty set.
emptySet :: Set
emptySet = 0

-- Checks if a set is empty.
setEmpty :: Set -> Bool
setEmpty n = n == 0

-- Generates a full set of a certain size.
fullSet :: Int -> Set
fullSet n
  | (n >= 0) && (n <= maxSet) = 2 ^ (n + 1) - 2
  | otherwise = error ("fullset:illegal set =" ++ show n)

-- Adds an element to the set
addSet :: Int -> Set -> Set
addSet i s = d' * e + m
  where
    (d, m) = divMod s e
    e = 2 ^ i
    d' = if odd d then d else d + 1

-- Deletes an element from the set at the specified index.
delSet :: Int -> Set -> Set
delSet i s = d' * e + m
  where
    (d, m) = divMod s e
    e = 2 ^ i
    d' = if odd d then d - 1 else d

-- Converts a set back to a list of integers
set2List :: Set -> [Int]
set2List s = s2l s 0
  where
    s2l 0 _ = []
    s2l n i
      | odd n = i : s2l (n `div` 2) (i + 1)
      | otherwise = s2l (n `div` 2) (i + 1)

-- The maximum number of elements that can be stored in the set.
maxSet :: Int
maxSet = truncate (logBase 2 (fromIntegral (maxBound :: Int))) - 1

-- Defines a new Table data type
newtype Table a b = Tbl (Data.Array.Array b a)
  deriving (Show)

-- Creates a new table
newTable :: (Data.Array.Ix b) => [(b, a)] -> Table a b
newTable l = Tbl (Data.Array.array (lo, hi) l)
  where
    indices = map fst l
    lo = minimum indices
    hi = maximum indices

-- Finds an element in a table by its index
findTable :: (Data.Array.Ix b) => Table a b -> b -> a
findTable (Tbl a) i = a Data.Array.! i

-- Updates a table with a given pair
updTable :: (Data.Array.Ix b) => (b, a) -> Table a b -> Table a b
updTable p@(i, x) (Tbl a) = Tbl (a Data.Array.// [p])

-- Converts a RoadMap to a adjacency matrix. Uses 1 as the first index.
createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = Data.Array.array bounds [((i, j), if i == j then Just 0 else distance rm (citiesList !! (i - 1)) (citiesList !! (j - 1))) | i <- [1 .. n], j <- [1 .. n]]
  where
    citiesList = cities rm
    n = length citiesList
    bounds = ((1, 1), (n, n))

-- Gets all the nodes from an adjacency matrix.
nodes :: AdjMatrix -> [Int]
nodes g = Data.Array.range (1, u) where ((1, _), (u, _)) = Data.Array.bounds g

-- Creates a table and uses dynamic programming to calculate each necessary entry in the table.
dynamic :: (Data.Array.Ix coord) => (Table entry coord -> coord -> entry) -> (coord, coord) -> Table entry coord
dynamic compute bnds = t
  where
    t = newTable (map (\coord -> (coord, compute t coord)) (Data.Array.range bnds))

-- Gets the distance (weight) by indexing the adjacency matrix.
-- The first 2 arguments represent the index of each city, and the third is the adjacency matrix.
weight :: Int -> Int -> AdjMatrix -> Distance
weight a b g =
  let w = g Data.Array.! (a, b)
   in case w of
        Just value -> value
        Nothing -> maxBound

-- The coordinate of the table
type TspCoord = (Int, Set)

-- The entry of the table
type TspEntry = (Int, [Int])

-- Computes the TSP entry for a given node and set of nodes.
-- The first argument is the adjacency matrix, followed by the destination node, the table and an auxiliary tuple
compTsp :: AdjMatrix -> Int -> Table TspEntry TspCoord -> TspCoord -> TspEntry
compTsp g n a (i, k)
  | setEmpty k = if weight i n g < maxBound then (weight i n g, [i, n]) else (maxBound, []) -- If the set is empty, its a direct edge from `i` to `n`
  | otherwise = if null paths then (maxBound, []) else minimum paths -- Otherwise, compute the paths from `i` to `n` passing through `k`
  where
    paths =
      [ (cost, i : rest)
        | j <- set2List k,
          let edgeDistance = weight i j g,
          edgeDistance < maxBound,
          let (partialCost, rest) = findTable a (j, delSet j k),
          not (null rest),
          let cost = edgeDistance + partialCost
      ]

-- Generates the bounds for the TSP table (1 to n)
bndsTsp :: Int -> ((Int, Set), (Int, Set))
bndsTsp n = ((1, emptySet), (n, fullSet (n - 1)))

-- Calculates the final path
tsp :: AdjMatrix -> (Int, [Int])
tsp g
  | n == 0 = (0, [])
  | n == 1 = (0, [0])
  | otherwise = findTable t (n, fullSet (n - 1))
  where
    n = length (nodes g)
    t = dynamic (compTsp g n) (bndsTsp n)

-- Calculates the TSP for a given RoadMap, returning the path.
travelSales :: RoadMap -> Path
travelSales rm =
  let adjMatrix = createAdjMatrix rm
      (totalDist, indicesPath) = tsp adjMatrix
      citiesList = cities rm
      path = map ((citiesList !!) . (\i -> i - 1)) indicesPath -- Convert 1-based index to 0-based
   in path

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]

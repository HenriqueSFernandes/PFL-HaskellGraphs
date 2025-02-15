import Control.Exception (evaluate)
import Proj1
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "cities" $ do
    it "Returns all cities in a gTest1" $ do
      cities gTest1 `shouldBe` ["0", "1", "2", "3", "4", "5", "6", "7", "8"]
    it "Returns all cities in a gTest2" $ do
      cities gTest2 `shouldBe` ["0", "1", "2", "3"]
    it "Returns all cities in a gTest3" $ do
      cities gTest3 `shouldBe` ["0", "1", "2", "3"]
    it "Returns all cities on a single edge roadmap" $ do
      cities [("1", "2", 5)] `shouldBe` ["1", "2"]
    it "Returns all cities on a roadmap with duplicate edges" $ do
      cities [("1", "2", 5), ("1", "2", 5)] `shouldBe` ["1", "2"]
    it "Returns empty list on an empty roadmap" $ do
      cities ([] :: RoadMap) `shouldBe` []

  describe "areAdjacent" $ do
    it "Returns True for two directly connected cities" $ do
      areAdjacent gTest1 "0" "1" `shouldBe` True
    it "Returns True for two cities connected in reverse order" $ do
      areAdjacent gTest1 "1" "0" `shouldBe` True
    it "Returns False for two cities that are not connected" $ do
      areAdjacent gTest1 "0" "2" `shouldBe` False
    it "Returns False for cities in an unconnected graph" $ do
      areAdjacent gTest3 "0" "2" `shouldBe` False
    it "Returns True for cities connected by a direct path in gTest2" $ do
      areAdjacent gTest2 "0" "1" `shouldBe` True
    it "Returns True for multiple connections between the same cities" $ do
      areAdjacent [("A", "B", 5), ("B", "A", 10), ("A", "C", 2)] "A" "B" `shouldBe` True
    it "Returns False for cities with no edges" $ do
      areAdjacent [] "A" "B" `shouldBe` False
    it "Returns False for same city" $ do
      areAdjacent [("A", "B", 5)] "A" "A" `shouldBe` False
    it "Returns False for same city where there is an error on the roadmap (connection between the same city)" $ do
      areAdjacent [("A", "A", 5)] "A" "A" `shouldBe` False

  describe "distance" $ do
    it "Returns Just distance for two directly connected cities" $ do
      distance gTest1 "0" "1" `shouldBe` Just 4

    it "Returns Just distance for two cities connected in reverse order" $ do
      distance gTest1 "1" "0" `shouldBe` Just 4

    it "Returns Nothing for two cities that are not connected" $ do
      distance gTest1 "0" "2" `shouldBe` Nothing

    it "Returns Nothing for cities in an unconnected graph" $ do
      distance gTest3 "0" "2" `shouldBe` Nothing

    it "Returns Just distance for cities connected by a direct path in gTest2" $ do
      distance gTest2 "0" "1" `shouldBe` Just 10

    it "Returns Nothing for cities with no edges" $ do
      distance [] "A" "B" `shouldBe` Nothing

  describe "adjacent" $ do
    it "Returns adjacent cities with distances for a city in gTest1" $ do
      adjacent gTest1 "0" `shouldBe` [("1", 4), ("7", 8)]

    it "Returns adjacent cities with distances for a city in gTest2" $ do
      adjacent gTest2 "0" `shouldBe` [("1", 10), ("2", 15), ("3", 20)]

    it "Returns adjacent cities with distances for a city in gTest3" $ do
      adjacent gTest3 "0" `shouldBe` [("1", 4)]

    it "Returns an empty list for a roadmap with connection across the same city" $ do
      adjacent [("4", "4", 5)] "4" `shouldBe` []

    it "Returns distances for cities connected by edges in the opposite direction" $ do
      adjacent gTest1 "6" `shouldBe` [("7", 1), ("5", 2), ("8", 6)]

    it "Returns an empty list for a city not in the graph" $ do
      adjacent gTest1 "X" `shouldBe` []

    it "Returns empty list for an empty graph" $ do
      adjacent [] "A" `shouldBe` []

  describe "pathDistance" $ do
    it "Returns Just total distance for a valid path" $ do
      pathDistance gTest1 ["0", "1", "2"] `shouldBe` Just 12 -- 4 (0 to 1) + 8 (1 to 2)
    it "Returns Nothing for an invalid path with a break in connections" $ do
      pathDistance gTest1 ["0", "2", "3"] `shouldBe` Nothing -- 0 to 2 is not directly connected
    it "Returns Just total distance for a valid path in a fully connected graph" $ do
      pathDistance gTest2 ["0", "1", "3"] `shouldBe` Just 35 -- 10 (0 to 1) + 25 (1 to 3)
    it "Returns Nothing for a path with non-existent cities" $ do
      pathDistance gTest1 ["0", "1", "X"] `shouldBe` Nothing -- X does not exist in the graph
    it "Returns Just 0 for a path with a single city" $ do
      pathDistance gTest1 ["0"] `shouldBe` Just 0 -- Distance of a single city is 0
    it "Returns Nothing for a path across same city (\"A\" -> \"A\")" $ do
      pathDistance gTest1 ["0", "0"] `shouldBe` Nothing
    it "Returns Nothing for an empty path" $ do
      pathDistance gTest1 [] `shouldBe` Nothing -- No path means no distance
    it "Returns Nothing for an emtpy graph" $ do
      pathDistance [] ["1", "2"] `shouldBe` Nothing

  describe "rome" $ do
    it "Returns the cities with the highest number of roads in gTest1" $ do
      rome gTest1 `shouldBe` ["2", "5", "7"]
    it "Returns the city with the highest number of roads in gTest2" $ do
      rome gTest2 `shouldBe` ["0", "1", "2", "3"]
    it "Returns the city with the highest number of roads in gTest3" $ do
      rome gTest3 `shouldBe` ["0", "1", "2", "3"]
    it "Returns only one city with the highest number of roads" $ do
      rome [("1", "2", 3), ("1", "3", 3), ("1", "4", 3)] `shouldBe` ["1"]
    it "Returns an empty list for an empty roadmap" $ do
      rome [] `shouldBe` [] -- No cities in an empty roadmap
    it "Returns the city with the highest number of roads when there's a self-loop" $ do
      let testRoadMap = [("1", "1", 5), ("1", "2", 3), ("1", "3", 4)]
      rome testRoadMap `shouldBe` ["1"] -- This should never happen but yeah better safe than sorry
  describe "isStronglyConnected" $ do
    it "Returns True for a strongly connected graph (gTest1)" $ do
      isStronglyConnected gTest1 `shouldBe` True
    it "Returns True for a fully connected graph (gTest2)" $ do
      isStronglyConnected gTest2 `shouldBe` True
    it "Returns False for a disconnected graph (gTest3)" $ do
      isStronglyConnected gTest3 `shouldBe` False
    it "Returns True for a single city" $ do
      isStronglyConnected [("A", "A", 1)] `shouldBe` True
    it "Returns True for a single edge" $ do
      isStronglyConnected [("A", "B", 1)] `shouldBe` True
    it "Returns False for an empty roadmap" $ do
      isStronglyConnected [] `shouldBe` False
    it "Returns True for a circular graph" $ do
      isStronglyConnected [("A", "B", 1), ("B", "C", 1), ("C", "A", 1)] `shouldBe` True -- All cities are reachable in a cycle

  describe "shortestPath" $ do
    it "Returns the shortest path between two directly connected cities" $ do
      shortestPath gTest1 "0" "1" `shouldBe` [["0", "1"]]  -- Direct connection

    it "Returns the shortest path between two cities with multiple paths" $ do
      let testRoadMap = [("0", "1", 5), ("1", "2", 2), ("0", "2", 8)]
      shortestPath testRoadMap "0" "2" `shouldBe` [["0", "1", "2"]]  -- Shortest path via city "1"

    it "Returns multiple shortest paths when they have the same distance" $ do
      let testRoadMap = [("A", "B", 1), ("B", "C", 1), ("A", "C", 2)]
      shortestPath testRoadMap "A" "C" `shouldBe` [["A", "B", "C"], ["A", "C"]]  -- Two shortest paths

    it "Returns the path from a city to itself" $ do
      shortestPath gTest1 "0" "0" `shouldBe` [["0"]]  -- Shortest path is just the city itself

    it "Returns an empty list if there is no path between two cities" $ do
      shortestPath gTest3 "0" "3" `shouldBe` []  -- No path between cities in gTest3

    it "Returns an empty list for two cities that are isolated" $ do
      let isolatedGraph = [("1", "2", 1), ("3", "4", 2)]
      shortestPath isolatedGraph "1" "3" `shouldBe` []  -- Cities are not connected

    it "Handles a large graph with one shortest path" $ do
      let largeGraph = [("A", "B", 1), ("B", "C", 2), ("C", "D", 1), ("A", "D", 5)]
      shortestPath largeGraph "A" "D" `shouldBe` [["A", "B", "C", "D"]]  -- One shortest path


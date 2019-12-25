import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2
import qualified AOC2_1
import qualified AOC2_2
import qualified AOC3_1
import qualified AOC3_2
import qualified AOC4_1
import qualified AOC4_2
import qualified AOC5_1
import qualified AOC5_2
import qualified AOC6_1
import qualified AOC6_2
import qualified AOC7_1

withInput = it "GOLD STAR *"

main :: IO ()
main = hspec . parallel $ do
  describe "1_1" $ do
    withInput $ do
      input <- readFile "test/AOC1.txt"
      AOC1_1.solve input `shouldBe` 3454942
  describe "1_2" $ do
    withInput $ do
      input <- readFile "test/AOC1.txt"
      AOC1_2.solve input `shouldBe` 5179544
  describe "2_1" $ do
    withInput $ do
      input <- readFile "test/AOC2.txt"
      AOC2_1.solve input `shouldBe` 5290681
  describe "2_2" $ do
    withInput $ do
      input <- readFile "test/AOC2.txt"
      AOC2_2.solve input `shouldBe` 5741
  describe "3_1" $ do
    withInput $ do
      input <- readFile "test/AOC3.txt"
      AOC3_1.solve input `shouldBe` 557
  describe "3_2" $ do
    withInput $ do
      input <- readFile "test/AOC3.txt"
      AOC3_2.solve input `shouldBe` 56410
  describe "4_1" $ do
    withInput $
      (AOC4_1.solve 245182 790572) `shouldBe` 1099
  describe "4_2" $ do
    withInput $
      (AOC4_2.solve 245182 790572) `shouldBe` 710
  describe "5_1" $ do
    withInput $ do
      input <- readFile "test/AOC5.txt"
      AOC5_1.solve input `shouldBe` 5346030
  describe "5_2" $ do
    it "should handle eq with position mode when positive" $
      (AOC5_2.solve 8 "3,9,8,9,10,9,4,9,99,-1,8") `shouldBe` 1
    it "should handle eq with position mode when negative" $
      (AOC5_2.solve 4 "3,9,8,9,10,9,4,9,99,-1,8") `shouldBe` 0
    it "should handle lt with position mode when positive" $
      (AOC5_2.solve 4 "3,9,7,9,10,9,4,9,99,-1,8") `shouldBe` 1
    it "should handle lt with position mode when negative" $
      (AOC5_2.solve 9 "3,9,7,9,10,9,4,9,99,-1,8") `shouldBe` 0
    it "should handle eq with immediate mode when positive" $
      (AOC5_2.solve 8 "3,3,1108,-1,8,3,4,3,99") `shouldBe` 1
    it "should handle eq with immediate mode when negative" $
      (AOC5_2.solve 4 "3,3,1108,-1,8,3,4,3,99") `shouldBe` 0
    it "should handle lt with immediate mode when positive" $
      (AOC5_2.solve 4 "3,3,1107,-1,8,3,4,3,99") `shouldBe` 1
    it "should handle lt with immediate mode when negative" $
      (AOC5_2.solve 9 "3,3,1107,-1,8,3,4,3,99") `shouldBe` 0
    it "should jump correctly using position mode, input 0" $
      (AOC5_2.solve 0 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") `shouldBe` 0
    it "should jump correctly using position mode, input non-0" $
      (AOC5_2.solve 14 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") `shouldBe` 1
    it "should jump correctly using immediate mode, input 0" $
      (AOC5_2.solve 0 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") `shouldBe` 0
    it "should jump correctly using immediate mode, input non-0" $
      (AOC5_2.solve 14 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") `shouldBe` 1
    withInput $ do
      input <- readFile "test/AOC5.txt"
      (AOC5_2.solve 5 input) `shouldBe` 513116
  describe "6_1" $ do
    it "should handle a simple example" $
      AOC6_1.solve "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L" `shouldBe` 42
    withInput $ do
      input <- readFile "test/AOC6.txt"
      AOC6_1.solve input `shouldBe` 249308
  describe "6_2" $ do
    withInput $ do
      input <- readFile "test/AOC6.txt"
      AOC6_2.solve input `shouldBe` 349
  describe "7_1" $ do
    it "should handle a simple example" $
      AOC7_1.solve "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" `shouldBe` 43210
    withInput $ do
      input <- readFile "test/AOC7.txt"
      AOC7_1.solve input `shouldBe` 38834
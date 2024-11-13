{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "parseQuery StartProduction" $
      Lib2.parseQuery "Start Production Hexacopter Motor Battery" @?= 
      Right (Lib2.StartProduction Lib2.Hexacopter [Lib2.Motor, Lib2.Battery]),
      
    testCase "parseQuery CheckInventory" $
      Lib2.parseQuery "Check Inventory Motor Battery" @?= 
      Right (Lib2.CheckInventory [Lib2.Motor, Lib2.Battery]),
      
    testCase "parseQuery PerformQualityCheck Battery Test" $
      Lib2.parseQuery "Perform Quality Check Battery Test" @?= 
      Right (Lib2.PerformQualityCheck Lib2.BatteryTest),
      
    testCase "parseQuery PerformQualityCheck Flight Test" $
      Lib2.parseQuery "Perform Quality Check Flight Test" @?= 
      Right (Lib2.PerformQualityCheck Lib2.FlightTest),
      
    testCase "parseQuery PerformQualityCheck Frame Inspection" $
      Lib2.parseQuery "Perform Quality Check Frame Inspection" @?= 
      Right (Lib2.PerformQualityCheck Lib2.FrameInspection),
      
    testCase "parseQuery Invalid" $
      Lib2.parseQuery "Invalid Query" @?= Left "Invalid query"
  ]
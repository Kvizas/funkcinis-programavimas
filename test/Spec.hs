{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Control.Concurrent (Chan, newChan, forkIO)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)

import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lib3Tests]

lib3Tests :: TestTree
lib3Tests = testGroup "Lib3 tests"
  [ testCase "SaveCommand state transition" $ do
      initialState <- newTVarIO Lib2.emptyState
      ioChan <- newChan
      _ <- forkIO $ Lib3.storageOpLoop ioChan
      result <- Lib3.stateTransition initialState Lib3.SaveCommand ioChan
      result @?= Right (Just "State saved successfully")

  , testCase "StatementCommand state transition" $ do
      initialState <- newTVarIO Lib2.emptyState
      ioChan <- newChan
      _ <- forkIO $ Lib3.storageOpLoop ioChan
      let statements = Lib3.Single (Lib2.AddComponent Lib2.Motor)
      result <- Lib3.stateTransition initialState (Lib3.StatementCommand statements) ioChan
      updatedState <- readTVarIO initialState
      result @?= Right (Just "Added Motor")
      Lib2.inventory updatedState @?= [Lib2.Motor]

  , testCase "renderStatements and parseStatements" $ do
      let statements = Lib3.Batch 
            [ Lib2.AddComponent Lib2.Motor
            , Lib2.StartProduction Lib2.Quadcopter [Lib2.Motor, Lib2.Battery]
            ]
      let rendered = Lib3.renderStatements statements
      let parsed = Lib3.parseStatements rendered
      parsed @?= Right (statements, "")

  , testCase "parseCommand with StatementCommand" $ do
      let input = "produce(Quadcopter Motor, Battery)"
      let expected = Right (Lib3.StatementCommand (Lib3.Single 
            (Lib2.StartProduction Lib2.Quadcopter [Lib2.Motor, Lib2.Battery])), "")
      Lib3.parseCommand input @?= expected

  , testCase "parseCommand with LoadCommand" $ do
      let input = "load"
      let expected = Right (Lib3.LoadCommand, "")
      Lib3.parseCommand input @?= expected

  , testCase "parseCommand with SaveCommand" $ do
      let input = "save"
      let expected = Right (Lib3.SaveCommand, "")
      Lib3.parseCommand input @?= expected

  , testCase "parseCommand with batch statements" $ do
      let input = "BEGIN\nadd_component(Motor);\nproduce(Quadcopter Motor, Battery);\nEND\n"
      let expected = Right (Lib3.StatementCommand (Lib3.Batch 
            [ Lib2.AddComponent Lib2.Motor
            , Lib2.StartProduction Lib2.Quadcopter [Lib2.Motor, Lib2.Battery]
            ]), "")
      Lib3.parseCommand input @?= expected
  ]
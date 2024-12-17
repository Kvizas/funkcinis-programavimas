{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command (..),
    Statements (..),
    atomicStatements
  )
where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Data.List (intersperse, intercalate)
import qualified Lib2
import Parsers
import System.Directory (doesFileExist)

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile "drone_state.txt" s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist "drone_state.txt"
      if exists
        then do
          s' <- readFile "drone_state.txt"
          writeChan chan s'
        else writeChan chan ""

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Show, Eq)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

parseCommand :: String -> Either String (Command, String)
parseCommand str = case parse (parseSave <|> parseLoad <|> StatementCommand <$> statements) str of
  (Left e, _) -> Left e
  (Right c, r) -> Right (c, r)

parseLoad :: Parser Command
parseLoad = do
  _ <- parseLiteral "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- parseLiteral "save"
  return SaveCommand

parseStatements :: String -> Either String (Statements, String)
parseStatements str = case parse statements str of
  (Left e, _) -> Left e
  (Right s, r) -> Right (s, r)

statements :: Parser Statements
statements =
  ( do
      _ <- parseLiteral "BEGIN"
      _ <- parseLiteral "\n"
      q <-
        many
          ( do
              q <- parseTask
              _ <- parseLiteral ";"
              _ <- parseLiteral "\n"
              return q
          )
      _ <- parseLiteral "END"
      _ <- parseLiteral "\n"
      return $ Batch q
  )
    <|> (Single <$> parseTask)

marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    currentInventory = Lib2.inventory state
    productionComponents = concatMap snd (Lib2.productionLog state)

    allComponents = currentInventory ++ productionComponents
    addComponentQueries = map Lib2.AddComponent allComponents

    productionQueries = map (\(model, components) -> Lib2.StartProduction model components) (Lib2.productionLog state)
    queries = addComponentQueries ++ productionQueries

renderStatements :: Statements -> String
renderStatements = \case
  Batch qs -> "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"
  Single q -> renderQuery q

renderQuery :: Lib2.Query -> String
renderQuery = \case
  Lib2.StartProduction model components -> 
    "produce(" ++ show model ++ " " ++ intercalate ", " (map show components) ++ ")"
  Lib2.CheckInventory components -> 
    "check_inventory(" ++ unwords (map show components) ++ ")"
  Lib2.PerformQC checks -> 
    "quality_check(" ++ intercalate ", " (map show checks) ++ ")"
  Lib2.ListModelsInProduction -> 
    "list_models_in_production()"
  Lib2.AddComponent component -> 
    "add_component(" ++ show component ++ ")"
  Lib2.Debug -> 
    "debug()"
  Lib2.Sequence queries ->
    "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) queries ++ "END"

stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if null qs
    then return (Left "No state file found")
    else case parseStatements qs of
      Left e -> return $ Left $ "Failed to load state from file:\n" ++ e
      Right (qs', _) -> atomically $ atomicStatements s qs'
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatements s sts

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList s (q : qs) = case Lib2.stateTransition s q of
  Left e -> Left e
  Right (msg, ns) ->
    if null qs
      then Right (msg, ns)
      else case transitionThroughList ns qs of
        Left e -> Left e
        Right (msg', ns') -> Right ((\x y -> x ++ "\n" ++ y) <$> msg <*> msg', ns')

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s statements = do
  s' <- readTVar s
  case transitionThroughList s' (toQueryList statements) of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
  where
    toQueryList (Batch qs) = qs
    toQueryList (Single q) = [q]
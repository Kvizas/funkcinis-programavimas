{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsers
  ( Query (..)
  , DroneModel (..)
  , Component (..)
  , Check (..)
  , Parser (..)
  , parseTaskList
  , parseTask
  , parseStartProduction
  , parseCheckInventory
  , parsePerformQC
  , parseListModels
  , parseAddComponent
  , parseDebug
  , skipSpaces
  , parseLiteral
  , parseString
  , parseInt
  , parseFloat
  , parseChar
  , char
  , Parser
  , parse
  , parseDroneModel
  , parseComponent
  , parseComponents
  , parseItemList
  , parseCheck
  , parseCheckList
  )
where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Alternative (empty), optional, (<|>))
import Data.Char (isDigit)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

data Query 
  = StartProduction DroneModel [Component] 
  | CheckInventory [Component]            
  | PerformQC [Check]             
  | ListModelsInProduction
  | AddComponent Component
  | Debug 
  | Sequence [Query]
  deriving (Show, Eq)

parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (char ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

parseTask :: Parser Query
parseTask =
  parseStartProduction
    <|> parseCheckInventory
    <|> parsePerformQC
    <|> parseListModels
    <|> parseAddComponent
    <|> parseDebug

parseStartProduction :: Parser Query
parseStartProduction = do
  _ <- parseLiteral "produce"
  _ <- parseLiteral "("
  model <- parseDroneModel
  components <- parseComponents
  _ <- parseLiteral ")"
  return $ StartProduction model components

parseCheckInventory :: Parser Query
parseCheckInventory = do
  _ <- parseLiteral "check_inventory"
  _ <- parseLiteral "("
  components <- parseItemList
  _ <- parseLiteral ")"
  return $ CheckInventory components

parsePerformQC :: Parser Query
parsePerformQC = do
  _ <- parseLiteral "quality_check"
  _ <- parseLiteral "("
  checks <- parseCheckList
  _ <- parseLiteral ")"
  return $ PerformQC checks

parseListModels :: Parser Query
parseListModels = do
  _ <- parseLiteral "list_models_in_production"
  _ <- parseLiteral "("
  _ <- parseLiteral ")"
  return ListModelsInProduction

parseAddComponent :: Parser Query
parseAddComponent = do
  _ <- parseLiteral "add_component"
  _ <- parseLiteral "("
  component <- parseComponent
  _ <- parseLiteral ")"
  return $ AddComponent component
  
sat :: (Char -> Bool) -> Parser Char
sat p = do
  input <- lift get
  case input of
    [] -> throwError "Empty String"
    (x:xs) -> if p x
              then lift (put xs) >> return x
              else throwError $ "Could not recognize: " ++ [x]
              
char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = lift (modify (dropWhile (== ' ')))

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseFloat :: Parser Float
parseFloat = do
  input <- lift get
  let (digits, rest) = span (\c -> c == '.' || isDigit c) (skipSpaces input)
  if null digits
    then throwError "Expected a float"
    else do
      lift (put rest)
      return (read digits)


parseString :: Parser String
parseString = do
  input <- lift get
  let input' = skipSpaces input
  if null input'
    then return ""
    else if head input' == '"'
         then parseQuotedString (tail input')
         else let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
              in lift (put rest) >> return str
  where
    parseQuotedString [] = throwError "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseInt :: Parser Int
parseInt = do
  input <- lift get
  let (digits, rest) = span isDigit (skipSpaces input)
  if null digits
    then throwError "Expected an integer"
    else do
      lift (put rest)
      return (read digits)

data DroneCommand
  = ProductionLine DroneModel [Component]
  | InventoryManagement [Component]
  | QualityControl [Check]
  deriving (Show, Eq)

data DroneModel
  = Hexacopter
  | Quadcopter
  | VTOLDrone
  | FixedWingDrone
  deriving (Show, Eq)

data Component
  = Motor
  | Battery
  | Frame
  | Propeller
  | Controller
  deriving (Show, Eq, Ord)

data Check
  = BatteryTest
  | FlightTest
  | FrameInspection
  deriving (Show, Eq)

parseDroneCommand :: Parser DroneCommand
parseDroneCommand = parseProductionLine <|> parseInventoryManagement <|> parseQualityControl

parseProductionLine :: Parser DroneCommand
parseProductionLine = do
  _ <- parseLiteral "Start Production"
  model <- parseDroneModel
  components <- parseComponents
  _ <- parseLiteral "End Production"
  return $ ProductionLine model components

parseInventoryManagement :: Parser DroneCommand
parseInventoryManagement = do
  _ <- parseLiteral "Check Inventory"
  components <- parseItemList
  return $ InventoryManagement components

parseQualityControl :: Parser DroneCommand
parseQualityControl = do
  _ <- parseLiteral "Perform Quality Check"
  checks <- parseCheckList
  return $ QualityControl checks

parseDroneModel :: Parser DroneModel
parseDroneModel = parseCopter <|> parsePlaneDrone

parseCopter :: Parser DroneModel
parseCopter = do
  model <- parseString
  return $ case model of
    "Hexacopter" -> Hexacopter
    "Quadcopter" -> Quadcopter
    _ -> error "Unknown copter model"

parsePlaneDrone :: Parser DroneModel
parsePlaneDrone = do
  model <- parseString
  return $ case model of
    "VTOL Drone" -> VTOLDrone
    "Fixed-Wing Drone" -> FixedWingDrone
    _ -> error "Unknown plane drone model"

parseComponent :: Parser Component
parseComponent = do
  skipSpaces'
  component <- parseString
  return $ case map (\c -> if c == '_' then ' ' else c) component of
    "Motor" -> Motor
    "Battery" -> Battery
    "Frame" -> Frame
    "Propeller" -> Propeller
    "Controller" -> Controller
    c -> error $ "Unknown component: " ++ c

parseComponents :: Parser [Component]
parseComponents = do
  firstComponent <- parseComponent
  rest <- optional (char ',' >> skipSpaces' >> parseComponents)
  return $ case rest of
    Just otherComponents -> firstComponent : otherComponents
    Nothing -> [firstComponent]

parseItemList :: Parser [Component]
parseItemList = parseComponents

parseCheck :: Parser Check
parseCheck = do
  skipSpaces'
  test <- parseString
  case test of
    "Battery" -> do
      _ <- skipSpaces'
      _ <- parseLiteral "Test"
      return BatteryTest
    "Flight" -> do
      _ <- skipSpaces'
      _ <- parseLiteral "Test"
      return FlightTest
    "Frame" -> do
      _ <- skipSpaces'
      _ <- parseLiteral "Inspection"
      return FrameInspection
    _ -> throwError $ "Unknown check: " ++ test

parseCheckList :: Parser [Check]
parseCheckList = do
  firstCheck <- parseCheck
  rest <- optional (char ',' >> skipSpaces' >> parseCheckList)
  return $ case rest of
    Just otherChecks -> firstCheck : otherChecks
    Nothing -> [firstCheck]

parseDebug :: Parser Query
parseDebug = do
  _ <- parseLiteral "debug"
  _ <- parseLiteral "("
  _ <- parseLiteral ")"
  return Debug
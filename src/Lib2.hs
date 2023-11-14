{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
    DroneModel (..),
    Component (..),
    Check (..),
    Parser
  )
where

import qualified Data.Char as C

type Parser a = String -> Either String (a, String)

data Query = StartProduction DroneModel [Component] | CheckInventory [Component] | PerformQualityCheck Check
  deriving (Show, Eq)

data DroneModel = Hexacopter | Quadcopter | VTOLDrone | FixedWingDrone
  deriving (Show, Eq)

data Component = Motor | Battery | Frame | Propeller | Controller
  deriving (Show, Eq)

data Check = BatteryTest | FlightTest | FrameInspection
  deriving (Show, Eq)

data State = State
  { inventory :: [Component],
    productionLog :: [(DroneModel, [Component])]
  }
  deriving (Show, Eq)

parseChar :: Char -> Parser Char
parseChar c [] = Left $ "Expected " ++ [c] ++ ", got empty input"
parseChar c (x:xs)
  | c == x = Right (c, xs)
  | otherwise = Left $ "Expected " ++ [c] ++ ", got " ++ [x]

parseString :: String -> Parser String
parseString [] input = Right ([], input)
parseString (x:xs) input = case parseChar x input of
  Right (_, rest) -> case parseString xs rest of
    Right (str, remaining) -> Right (x:str, remaining)
    Left e -> Left e
  Left e -> Left e

parseWhitespace :: Parser String
parseWhitespace [] = Right ("", [])
parseWhitespace (x:xs)
  | C.isSpace x = case parseWhitespace xs of
      Right (spaces, rest) -> Right (x:spaces, rest)
      Left e -> Left e
  | otherwise = Right ("", x:xs)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right result -> Right result
  Left _ -> p2 input

andThen :: Parser a -> Parser b -> Parser (a, b)
andThen p1 p2 input = case p1 input of
  Right (v1, rest1) -> case p2 rest1 of
    Right (v2, rest2) -> Right ((v1, v2), rest2)
    Left e -> Left e
  Left e -> Left e

token :: String -> Parser String
token s = \input -> case parseWhitespace input of
  Right (_, rest1) -> case parseString s rest1 of
    Right (result, rest2) -> case parseWhitespace rest2 of
      Right (_, rest3) -> Right (result, rest3)
      Left e -> Left e
    Left e -> Left e
  Left e -> Left e

parseDroneModel :: Parser DroneModel
parseDroneModel = \input -> case parseWhitespace input of
  Right (_, rest) -> 
    orElse
      (\i -> case parseString "Hexacopter" i of
        Right (_, r) -> Right (Hexacopter, r)
        Left e -> Left e)
      (orElse
        (\i -> case parseString "Quadcopter" i of
          Right (_, r) -> Right (Quadcopter, r)
          Left e -> Left e)
        (orElse
          (\i -> case parseString "VTOLDrone" i of
            Right (_, r) -> Right (VTOLDrone, r)
            Left e -> Left e)
          (\i -> case parseString "FixedWingDrone" i of
            Right (_, r) -> Right (FixedWingDrone, r)
            Left e -> Left e)))
      rest
  Left e -> Left e

parseComponent :: Parser Component
parseComponent = \input -> case parseWhitespace input of
  Right (_, rest) ->
    orElse
      (\i -> case parseString "Motor" i of
        Right (_, r) -> Right (Motor, r)
        Left e -> Left e)
      (orElse
        (\i -> case parseString "Battery" i of
          Right (_, r) -> Right (Battery, r)
          Left e -> Left e)
        (orElse
          (\i -> case parseString "Frame" i of
            Right (_, r) -> Right (Frame, r)
            Left e -> Left e)
          (orElse
            (\i -> case parseString "Propeller" i of
              Right (_, r) -> Right (Propeller, r)
              Left e -> Left e)
            (\i -> case parseString "Controller" i of
              Right (_, r) -> Right (Controller, r)
              Left e -> Left e))))
      rest
  Left e -> Left e

parseComponents :: Parser [Component]
parseComponents input = case parseComponent input of
  Right (comp, rest) -> case parseComponents rest of
    Right (comps, final) -> Right (comp:comps, final)
    Left _ -> Right ([comp], rest)
  Left _ -> Right ([], input)

parseCheck :: Parser Check
parseCheck = \input -> case parseWhitespace input of
  Right (_, rest) ->
    orElse
      (\i -> case parseString "Battery Test" i of
        Right (_, r) -> Right (BatteryTest, r)
        Left e -> Left e)
      (orElse
        (\i -> case parseString "Flight Test" i of
          Right (_, r) -> Right (FlightTest, r)
          Left e -> Left e)
        (\i -> case parseString "Frame Inspection" i of
          Right (_, r) -> Right (FrameInspection, r)
          Left e -> Left e))
      rest
  Left e -> Left e

parseQuery :: String -> Either String Query
parseQuery input = case token "Start Production" input of
  Right (_, rest1) -> case parseDroneModel rest1 of
    Right (model, rest2) -> case parseComponents rest2 of
      Right (comps, _) -> Right $ StartProduction model comps
      Left e -> Left e
    Left e -> Left e
  Left _ -> case token "Check Inventory" input of
    Right (_, rest1) -> case parseComponents rest1 of
      Right (comps, _) -> Right $ CheckInventory comps
      Left e -> Left e
    Left _ -> case token "Perform Quality Check" input of
      Right (_, rest1) -> case parseCheck rest1 of
        Right (check, _) -> Right $ PerformQualityCheck check
        Left e -> Left e
      Left _ -> Left "Invalid query"

emptyState :: State
emptyState = State {inventory = [], productionLog = []}

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  StartProduction model components -> 
    Right (Just $ "Started production of " ++ show model, st {productionLog = (model, components) : productionLog st})
  CheckInventory items -> 
    Right (Just $ "Current inventory: " ++ show (inventory st), st)
  PerformQualityCheck check -> 
    Right (Just $ "Performed quality check: " ++ show check, st)
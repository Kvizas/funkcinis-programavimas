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
    parseDroneModel,
    parseComponents,
    parseItemList,
    parseCheck,
    parseQuery,
  )
where

import qualified Data.List as L

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

-- <drone_model> ::= <copter> | <plane_drone>
parseDroneModel :: String -> Either String DroneModel
parseDroneModel "Hexacopter" = Right Hexacopter
parseDroneModel "Quadcopter" = Right Quadcopter
parseDroneModel "VTOLDrone" = Right VTOLDrone
parseDroneModel "FixedWingDrone" = Right FixedWingDrone
parseDroneModel _ = Left "Invalid drone model"

-- <components> ::= <component> | <component> <components>
parseComponents :: String -> Either String [Component]
parseComponents s = mapM parseComponent (words s)

-- <component> ::= "Motor" | "Battery" | "Frame" | "Propeller" | "Controller"
parseComponent :: String -> Either String Component
parseComponent "Motor" = Right Motor
parseComponent "Battery" = Right Battery
parseComponent "Frame" = Right Frame
parseComponent "Propeller" = Right Propeller
parseComponent "Controller" = Right Controller
parseComponent _ = Left "Invalid component"

-- <item_list> ::= <component> | <component> <item_list>
parseItemList :: String -> Either String [Component]
parseItemList s = parseComponents s

-- <check> ::= "Battery Test" | "Flight Test" | "Frame Inspection"
parseCheck :: String -> Either String Check
parseCheck "Battery Test" = Right BatteryTest
parseCheck "Flight Test" = Right FlightTest
parseCheck "Frame Inspection" = Right FrameInspection
parseCheck _ = Left "Invalid check"

-- User Input
parseQuery :: String -> Either String Query
parseQuery s = case words s of
  "Start" : "Production" : model : components -> case parseDroneModel model of
    Right model' -> case parseComponents (unwords components) of
      Right components' -> Right $ StartProduction model' components'
      Left e -> Left e
    Left e -> Left e
  "Check" : "Inventory" : items -> case parseItemList (unwords items) of
    Right items' -> Right $ CheckInventory items'
    Left e -> Left e
  "Perform" : "Quality" : "Check" : rest -> 
    case parseCheck (unwords rest) of
      Right check' -> Right $ PerformQualityCheck check'
      Left e -> Left e
  _ -> Left "Invalid query"

emptyState :: State
emptyState = State {inventory = [], productionLog = []}

-- | Updates a state according to a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  StartProduction model components -> 
    Right (Just $ "Started production of " ++ show model, st {productionLog = (model, components) : productionLog st})
  CheckInventory items -> 
    Right (Just $ "Current inventory: " ++ show (inventory st), st)
  PerformQualityCheck check -> 
    Right (Just $ "Performed quality check: " ++ show check, st)
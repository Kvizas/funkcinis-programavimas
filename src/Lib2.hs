{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
    DroneModel (..),
    Component (..),
    Check (..)
  )
where

import Parsers

data State = State
  { inventory :: [Component],
    productionLog :: [(DroneModel, [Component])]
  }
  deriving (Show, Eq)

-- Add required components for each drone type
requiredComponents :: DroneModel -> [Component]
requiredComponents Quadcopter = [Motor, Motor, Motor, Motor, Frame, Battery, Controller, Propeller, Propeller, Propeller, Propeller]
requiredComponents Hexacopter = [Motor, Motor, Motor, Motor, Motor, Motor, Frame, Battery, Controller, Propeller, Propeller, Propeller, Propeller, Propeller, Propeller]
requiredComponents VTOLDrone = [Motor, Motor, Motor, Motor, Frame, Battery, Controller, Propeller, Propeller, Propeller, Propeller]
requiredComponents FixedWingDrone = [Motor, Motor, Frame, Battery, Controller, Propeller, Propeller]

parseQuery :: String -> Either String Query
parseQuery s =
  case parse parseTaskList s of
    (Left e, _) -> Left e
    (Right qs, r) -> if null r
      then case qs of
        [q] -> Right q
        _ -> Right (Sequence qs)
      else Left ("Unrecognized characters: " ++ r)

emptyState :: State
emptyState = State {inventory = [], productionLog = []}

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  ListModelsInProduction -> 
    Right (Just $ show $ map fst $ productionLog st, st)
  
  CheckInventory components -> 
    Right (Just $ show $ inventory st, st)
  
  StartProduction model components ->
    let required = requiredComponents model
        missing = findMissingComponents required components
    in if not (null missing)
         then Left $ "Missing required components for " ++ show model ++ ": " ++ show missing
         else if canProduce components st
           then Right (Just $ "Started production of " ++ show model, 
                      st { productionLog = (model, components) : productionLog st,
                           inventory = removeComponents components st })
           else Left "Insufficient components in inventory"
  
  AddComponent component ->
    Right (Just $ "Added " ++ show component, 
           st { inventory = component : inventory st })
  
  PerformQC checks ->
    Right (Just $ "Performing checks: " ++ show checks, st)
  
  Debug -> 
    Right (Just $ show st, st)
  
  Sequence queries ->
    foldl processQuery (Right (Just "", st)) queries
    where
      processQuery (Left err) _ = Left err
      processQuery (Right (msg, state)) q = 
        case stateTransition state q of
          Left err -> Left err
          Right (Just newMsg, newState) -> Right (combineMessages msg (Just newMsg), newState)
          Right (Nothing, newState) -> Right (msg, newState)

canProduce :: [Component] -> State -> Bool
canProduce components st = all (`elem` inventory st) components

-- Helper function to find missing required components
findMissingComponents :: [Component] -> [Component] -> [Component]
findMissingComponents required provided =
  let countMap = foldr (\c m -> increment c m) [] required
      providedMap = foldr (\c m -> increment c m) [] provided
  in concatMap (\(comp, count) -> 
       replicate (count - findCount comp providedMap) comp) countMap
  where
    increment c [] = [(c, 1)]
    increment c ((x,n):xs) | c == x = (x,n+1):xs
                          | otherwise = (x,n):increment c xs
    
    findCount _ [] = 0
    findCount c ((x,n):xs) | c == x = n
                          | otherwise = findCount c xs

-- Update removeComponents to only remove the exact components needed
removeComponents :: [Component] -> State -> [Component]
removeComponents toRemove st =
  let countMap = foldr (\c m -> increment c m) [] toRemove
  in removeWithCounts countMap (inventory st)
  where
    increment c [] = [(c, 1)]
    increment c ((x,n):xs) | c == x = (x,n+1):xs
                          | otherwise = (x,n):increment c xs
    
    removeWithCounts _ [] = []
    removeWithCounts counts (x:xs)
      | shouldRemove x counts = removeWithCounts (decrementCount x counts) xs
      | otherwise = x : removeWithCounts counts xs
    
    shouldRemove c counts = findCount c counts > 0
    
    findCount _ [] = 0
    findCount c ((x,n):xs) | c == x = n
                          | otherwise = findCount c xs
    
    decrementCount c [] = []
    decrementCount c ((x,n):xs)
      | c == x = if n > 1 then (x,n-1):xs else xs
      | otherwise = (x,n):decrementCount c xs

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just m1) Nothing = Just m1
combineMessages Nothing (Just m2) = Just m2
combineMessages (Just m1) (Just m2) = Just (m1 ++ "\n" ++ m2)
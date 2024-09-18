module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["Start Production", "End Production", "Hexacopter", "Quadcopter", "VTOL Drone", "Fixed-Wing Drone", "Check Inventory", "Motor", "Battery", "Frame", "Propeller", "Controller", "Perform Quality Check", "Battery Test", "Flight Test", "Frame Inspection"]

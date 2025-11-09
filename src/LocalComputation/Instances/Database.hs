{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs        #-}
module LocalComputation.Instances.Database (

) where

import           Control.Monad.IO.Class                      (MonadIO)
import           Data.Functor                                (void)
import qualified Data.List                                   as L
import qualified Data.Set                                    as S
import qualified LocalComputation.Inference                  as I
import qualified LocalComputation.Inference.JoinTree.Diagram as D
import qualified LocalComputation.Inference.MessagePassing   as MP
import qualified LocalComputation.Pretty                     as P
import qualified LocalComputation.ValuationAlgebra           as V

--------------------------------------------------------------------------------
-- Datatype definition
--------------------------------------------------------------------------------
data Table a b = Table { headings :: [b], rows :: [[a]] } deriving (V.Generic, V.Binary, V.NFData)

type Database a b = [Table a b]

--------------------------------------------------------------------------------
-- Small Example problem
--------------------------------------------------------------------------------
table1 :: Table String String
table1 = Table {
      headings =  ["Name",  "Age",  "Birthplace"]
    , rows     = [
                  ["Ben",   "25",   "China"]
                , ["Julia", "28",   "Australia"]
                ]


}

table2 :: Table String String
table2 = Table {
      headings =  ["Name",  "ID"]
    , rows     = [
                  ["Ben",   "1"]
                , ["David", "2"]
                , ["Julia", "3"]
               ]
}

query :: S.Set String
query = S.fromList $ ["ID", "Age"]

--------------------------------------------------------------------------------
-- Instance definition
--------------------------------------------------------------------------------


instance (Table a b ~ Table String String) => V.ValuationFamily (Table a) where


    label :: V.Var c => Table String c -> S.Set c
    label t = S.fromList $ t.headings

    _combine :: V.Var c => Table String c -> Table String c -> Table String c
    _combine t1 t2 = naturalJoinTables t1 t2

    _project :: V.Var c => Table String c -> S.Set c -> Table String c
    _project t newDomain = t { headings = filter (`elem` newDomain) t.headings
                             , rows = map filterOnlyNewDomain t.rows
                             }
        where
            filterOnlyNewDomain row = [value | (heading, value) <- zip t.headings row, heading `elem` newDomain]

    identity :: Table String c
    identity = Table [] [[]]

    isIdentity :: Table String c -> Bool
    isIdentity (Table [] [[]]) = True
    isIdentity _               = False



--------------------------------------------------------------------------------
-- Answering
--------------------------------------------------------------------------------

answer :: (MonadIO m) => m (Table String String)
answer = I.unsafeQuery I.Fusion [table1, table2] query

drawGraph :: (MonadIO m) => m ()
drawGraph = void $ I.unsafeQueryDrawGraph draw (I.Shenoy MP.Threads) [table1, table2] query
    where
        draw = D.def { D.beforeInference = Just "diagrams/demo/before-inference.svg"
                     , D.afterInference = Just "diagrams/demo/after-inference.svg"
                    }


{-

>>> answer
Age  ID

25   1
28   3

25   1
28   3

-}

{-

>>> drawGraph

-}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
instance Show (Table String String) where
    show t
        | V.isIdentity t = "[Empty Table]"
        | otherwise = P.showTable $ P.unsafeTable t.headings t.rows


naturalJoinTables :: (Eq a, Ord b) => Table a b -> Table a b -> Table a b
naturalJoinTables t1 t2 = Table headings [map snd $ mergeRows t1row' t2row' | t1row <- t1.rows, t2row <- t2.rows,
                                                              let t1row' = zip t1.headings t1row,
                                                              let t2row' = zip t2.headings t2row,
                                                              numMatchingVariables t1row' t2row' == expectedMatchingVariables]
    where
        headings = L.sort $ t1.headings ++ filter (`notElem` t1.headings) t2.headings
        expectedMatchingVariables = length $ filter (`elem` t2.headings) t1.headings

mergeRows :: (Ord a) => [(a, b)] -> [(a, b)] -> [(a, b)]
mergeRows row otherRow = L.sortOn fst $ row ++ (filter (\(x, _) -> x `notElem` variablesAlreadyPresent) otherRow)
    where
        variablesAlreadyPresent = map fst row

numMatchingVariables :: (Eq a, Eq b) => [(a, b)] -> [(a, b)] -> Int
numMatchingVariables row otherRow = sum $ map (\entry -> case entry `elem` otherRow of
                                                                    True  -> 1
                                                                    False -> 0) row


--------------------------------------------------------------------------------
-- Large example problem (generated by ChatGPT 5)
--------------------------------------------------------------------------------
-- 1. Employees
employees :: Table String String
employees = Table {
  headings = ["EmpID", "Name",  "DeptID", "RoleID", "CountryCode"],
  rows = [
             ["E01",   "Alice", "D01",    "R01",    "AU"],
             ["E02",   "Ben",   "D03",    "R02",    "US"],
             ["E03",   "Carla", "D02",    "R03",    "AU"],
             ["E04",   "David", "D03",    "R01",    "JP"],
             ["E05",   "Eli",   "D04",    "R04",    "US"]
  ]
}

-- 2. Departments
departments :: Table String String
departments = Table {
  headings = ["DeptID", "DeptName",    "ManagerID"],
  rows = [
             ["D01",    "Engineering", "E01"],
             ["D02",    "HR",          "E03"],
             ["D03",    "Sales",       "E02"],
             ["D04",    "Finance",     "E05"]
  ]
}

-- 3. Roles
roles :: Table String String
roles = Table {
  headings = ["RoleID", "RoleTitle",     "SalaryBand"],
  rows = [
             ["R01",    "Engineer",      "SB2"],
             ["R02",    "HR Specialist", "SB1"],
             ["R03",    "Tech Lead",     "SB3"],
             ["R04",    "Accountant",    "SB2"]
  ]
}

-- 4. SalaryBands
salaryBands :: Table String String
salaryBands = Table {
  headings = ["SalaryBand", "MinSalary", "MaxSalary"],
  rows = [
             ["SB1",        "50000",     "70000"],
             ["SB2",        "70000",     "90000"],
             ["SB3",        "90000",     "120000"]
  ]
}

-- 5. Countries
countries :: Table String String
countries = Table {
  headings = ["CountryCode", "CountryName",   "RegionID"],
  rows = [
             ["AU",          "Australia",     "R01"],
             ["US",          "United States", "R02"],
             ["JP",          "Japan",         "R03"]
  ]
}

-- 6. Regions
regions :: Table String String
regions = Table {
  headings = ["RegionID", "RegionName",    "Continent"],
  rows = [
             ["R01",      "Oceania",       "Asia-Pacific"],
             ["R02",      "North America", "Americas"],
             ["R03",      "East Asia",     "Asia-Pacific"]
  ]
}

-- 7. Projects (each project belongs to a department)
projects :: Table String String
projects = Table {
  headings = ["ProjectID", "ProjectName",        "Budget"],
  rows = [
             ["P01",       "AI Platform",        "200000"],
             ["P02",       "Recruitment Portal", "100000"],
             ["P03",       "CRM Revamp",         "150000"],
             ["P04",       "Audit System",       "120000"]
  ]
}

-- 8. EmployeeProjects (many-to-many; employees can work on cross-dept projects)
employeeProjects :: Table String String
employeeProjects = Table {
  headings = ["EmpID", "ProjectID", "HoursPerWeek"],
  rows = [
             ["E01",   "P01",       "25"],
             ["E03",   "P01",       "10"],
             ["E02",   "P02",       "30"],
             ["E04",   "P03",       "40"],
             ["E05",   "P04",       "35"],
             ["E01",   "P03",       "15"]
  ]
}

-- 9. Trainings (which role requires which training)
trainings :: Table String String
trainings = Table {
  headings = ["TrainingID", "TrainingName",         "RequiredForRoleID"],
  rows = [
             ["T01",        "Leadership 101",       "R03"],  -- required for Tech Lead (R03)
             ["T02",        "Accounting Basics",    "R04"],  -- required for Accountant (R04)
             ["T03",        "Ethics and HR Policy", "R02"]   -- required for HR Specialist (R02)
  ]
}

-- 10. EmployeeTrainings (who completed what)
employeeTrainings :: Table String String
employeeTrainings = Table {
  headings = ["EmpID", "TrainingID", "CompletionStatus"],
  rows = [
             ["E01",   "T01",        "Completed"],    -- Alice completed Leadership (even if not R03)
             ["E03",   "T01",        "Completed"],    -- Carla completed Leadership (she's R03)
             ["E02",   "T03",        "InProgress"],   -- Ben (R02) in progress on Ethics/HR Policy
             ["E04",   "T01",        "InProgress"],   -- Ben (R02) in progress on Ethics/HR Policy
             ["E05",   "T02",        "Completed"]     -- Eli completed Accounting Basics (R04)
  ]
}

-- 11. Managers
managers :: Table String String
managers = Table {
  headings = ["ManagerID", "ManagerName"],
  rows = [
             ["E01",       "Alice"],
             ["E02",       "Ben"],
             ["E03",       "Carla"],
             ["E05",       "Eli"]
  ]
}

--------------------------------------------------------------------------------
-- Full database
--------------------------------------------------------------------------------
companyDB :: Database String String
companyDB =
  [ employees, departments, roles, salaryBands
  , countries, regions, projects, employeeProjects
  , trainings, employeeTrainings, managers
  ]

--------------------------------------------------------------------------------
-- Example Query (hard)
--------------------------------------------------------------------------------
largeQuery :: S.Set String
largeQuery = S.fromList
  [ "Name"
  , "ProjectName"
  , "RegionName"
  , "MaxSalary"
  ]


largeQueryAnswer :: (MonadIO m) => m (Table String String)
largeQueryAnswer = I.unsafeQuery I.Fusion companyDB largeQuery
{-

>>> largeQueryAnswer
MaxSalary  Name   ProjectName         RegionName

90000      Alice  AI Platform         Oceania
foobar :: ()
foobar = undefined
120000     Carla  AI Platform         Oceania
70000      Ben    Recruitment Portal  North America
90000      David  CRM Revamp          East Asia
90000      Alice  CRM Revamp          Oceania
90000      Eli    Audit System        North America

-}

largeQueryDrawGraph :: (MonadIO m) => m ()
largeQueryDrawGraph = void $ I.unsafeQueryDrawGraph draw (I.Shenoy MP.Threads) companyDB largeQuery
    where
        draw = D.def { D.beforeInference = Just "diagrams/demo/before-inference-large.svg"
                     , D.afterInference = Just "diagrams/demo/after-inference-large.svg"
                    }
{-

>>> largeQueryDrawGraph

-}

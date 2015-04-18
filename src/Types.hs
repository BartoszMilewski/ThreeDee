{-# LANGUAGE DeriveGeneric #-}

module Types (
    Vector2 (..), Point (..), Edge (..), Diagram (..), World (..)
) where

import Data.Aeson
import GHC.Generics

data Point = Point {
    n :: Int -- points are numbered
  , x :: Double
  , y :: Double
  , z :: Double
  , pcolor :: String
  , pLabel :: String
  } deriving (Show, Read, Generic)

data Edge = Edge {
  -- an edge connects two points
    n1 :: Int
  , n2 :: Int
  , ecolor :: String
  , eLabel :: String
  } deriving (Show, Read, Generic)


data Diagram = Diagram {
    points :: [Point]
  , edges  :: [Edge]
} deriving (Show, Read, Generic)

data Vector2 = Vector2 {
    vx :: Double, vy :: Double
  } deriving (Show, Read, Generic)

data World = World {
    seqNum     :: Int   -- sequence number to serialize communications
  , delta      :: Vector2 -- vector to be used to transform diagram
  , diagram    :: Diagram
  } deriving (Show, Read, Generic)

instance FromJSON Point
instance ToJSON   Point

instance FromJSON Edge
instance ToJSON   Edge

instance FromJSON Vector2
instance ToJSON   Vector2

instance FromJSON Diagram
instance ToJSON   Diagram

instance FromJSON World
instance ToJSON   World
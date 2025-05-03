{-# LANGUAGE DeriveGeneric #-}

module Statistics.Distribution.Pareto
    ( ParetoDistribution
    ) where

import GHC.Generics          (Generic)
import Numeric.MathFunctions.Constants (m_neg_inf)
import qualified Statistics.Distribution as D

-- TODO: Add instances for Typeable, Data?
data ParetoDistribution = ParetoDistribution 
    { scale :: Double
    , shape :: Double 
    } deriving (Eq, Generic)

{- TODO: Figure out how to adapt the following
instance Show NormalDistribution where
  showsPrec i (ND m s _ _) = defaultShow2 "normalDistr" m s i
instance Read NormalDistribution where
  readPrec = defaultReadPrecM2 "normalDistr" normalDistrE
-}

{- TODO: Implement ToJSON and FromJSON -}

instance D.Distribution ParetoDistribution where
  -- cumulative  d x     = 1 - complCumulative d x
  complCumulative d x = (scale d / x) ** shape d

instance D.MaybeMean ParetoDistribution where
    maybeMean d | shape d > 1 = Just $ scale d * shape d / (shape d - 1)
                | otherwise   = Nothing

{-
instance D.ContDistr ParetoDistribution where
    density (ParetoDistribution scale shape) x
      | x < scale = 0
      | otherwise = shape * (scale ** shape) / (x ** (shape + 1))
    logDensity (ParetoDistribution scale shape) x
      | x < scale = m_neg_inf
      | otherwise = log shape + shape * log scale - (shape + 1) * log x
    quantile      = quantile
    complQuantile = complQuantile
-}

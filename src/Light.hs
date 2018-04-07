module Light where

import Ray
import Numeric.LinearAlgebra

data Light =    PointLight {color :: Vector Double,
                            position :: Vector Double,
                            constantTerm :: Double,
                            linearTerm :: Double,
                            quadraticTerm :: Double}
            |   DirectionalLight {color :: Vector Double,
                                  orientation :: Vector Double}
    deriving (Show, Eq)

lightColor :: Light -> Vector Double
lightColor (PointLight c _ _ _ _) = c
lightColor (DirectionalLight c _) = c

getDirection :: Light -> Vector Double -> Vector Double
getDirection (PointLight _ pos _ _ _) p = dir / norm
    where dir = pos - p
          norm = (fromList [norm_2 dir]) :: Vector Double
getDirection (DirectionalLight _ o) _ = -o

distanceAttenuation :: Light -> Vector Double -> Double
distanceAttenuation (PointLight pos _ c b a) p = min 1.0 atten
    where atten = (c * dist * dist + b * dist + a) ** (-1.0)
          dist = norm_2 (p - pos)
distanceAttenuation (DirectionalLight _ _) p = 1.0

{- TODO: this requires scene implementation
shadowAttenuation :: Ray -> Vector Double -> Vector Double
-}

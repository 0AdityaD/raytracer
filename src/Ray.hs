module Ray where

import Numeric.LinearAlgebra

data Ray = Ray {position :: Vector Double,
                direction :: Vector Double}
    deriving (Show, Eq)

data Isect = Isect {t :: Double,
                    normal :: Vector Double,
                    material :: Material}

instance Show Isect where
    show (Isect t n m) = "time: " ++ show t ++ ", normal: " ++ show n

class At a where
    at :: a -> Ray -> Vector Double

instance At Double where
    at t (Ray p d) = p + (fromList [t] :: Vector Double) * d

instance At Isect where 
    at (Isect t _ _) (Ray p d) = p + (fromList [t] :: Vector Double) * d

type MatParam = Isect -> Vector Double

data Material = Material {ka :: MatParam,
                          ks :: MatParam,
                          kd :: MatParam,
                          ke :: MatParam,
                          kr :: MatParam,
                          kt :: MatParam}

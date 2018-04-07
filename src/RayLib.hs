module RayLib where

import Numeric.LinearAlgebra

data Ray = Ray {rayOrigin :: Vector Double,
                rayDirection :: Vector Double}
    deriving (Show, Eq)

data Isect = Isect {iT :: Double,
                    iNormal :: Vector Double,
                    iObject :: Geometry,
                    iMaterial :: Material}

instance Show Isect where
    show (Isect t n _ _) = "time: " ++ show t ++ ", normal: " ++ show n

class At a where
    at :: a -> Ray -> Vector Double

instance At Double where
    at t (Ray p d) = p + (fromList [t] :: Vector Double) * d

instance At Isect where 
    at (Isect t _ _ _) (Ray p d) = p + (fromList [t] :: Vector Double) * d

type MatParam = Isect -> Vector Double

data Material = Material {ka :: MatParam,
                          ks :: MatParam,
                          kd :: MatParam,
                          ke :: MatParam,
                          kr :: MatParam,
                          kt :: MatParam}

data Light =    PointLight       {lColor :: Vector Double,
                                  lPosition :: Vector Double,
                                  constantTerm :: Double,
                                  linearTerm :: Double,
                                  quadraticTerm :: Double}
            |   DirectionalLight {lColor :: Vector Double,
                                  lOrientation :: Vector Double}
    deriving (Show, Eq)

type Lights = [Light]

lightColor :: Light -> Vector Double
lightColor (PointLight c _ _ _ _) = c
lightColor (DirectionalLight c _) = c

getDirection :: Light -> Vector Double -> Vector Double
getDirection (PointLight _ pos _ _ _) p = normalize (pos - p)
getDirection (DirectionalLight _ o) _ = -o

distanceAttenuation :: Light -> Vector Double -> Double
distanceAttenuation (PointLight pos _ c b a) p = min 1.0 atten
    where atten = (c * dist * dist + b * dist + a) ** (-1.0)
          dist = norm_2 (p - pos)
distanceAttenuation (DirectionalLight _ _) p = 1.0

data Geometry = Sphere {sphereCenter :: Vector Double,
                        sphereRadius :: Double,
                        sphereMaterial :: Material}

instance Show Geometry where
    show (Sphere center radius material) =
        show "center: " ++ show center ++ ", radius" ++ show radius

type Objects = [Geometry]

data Scene = Scene {lights :: Lights,
                    objects :: Objects,
                    ambient :: Vector Double}

intersect :: Geometry -> Ray -> Maybe Isect
intersect s@(Sphere center radius material) r@(Ray rayPos rayDir) =
    let v = center - rayPos in
    let b = v <.> rayDir in
    let discriminant = b * b - v <.> v + radius * radius in
    if discriminant < 0.0 then
        Nothing
    else
        let det = sqrt discriminant in
        let (t1,t2) = (b - det, b + det) in
        if t2 < 0.0 then
            Nothing
        else if t1 > 0.0 then
            pure (Isect t1 (at t1 r) s material)
        else
            pure (Isect t2 (at t2 r) s material)


{- TODO: this requires scene implementation
shadowAttenuation :: Ray -> Vector Double -> Vector Double
-}

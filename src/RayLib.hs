module RayLib where

import Numeric.LinearAlgebra
import qualified Graphics.Image as G
import qualified Debug.Trace as Debug

data Ray = Ray {rayOrigin :: Vector Double,
                rayDirection :: Vector Double}
    deriving (Show, Eq)

eps :: Double
eps = 0.000001

data Isect = Isect {iT :: Double,
                    iNormal :: Vector Double,
                    iObject :: Geometry,
                    iMaterial :: Material}

instance Eq Isect where
    (Isect iT1 iNormal1 _ _) == (Isect iT2 iNormal2 _ _) = (iT1 == iT2) && (iNormal1 == iNormal2)

instance Ord Isect where
    compare (Isect iT1 _ _ _) (Isect iT2 _ _ _) = compare iT1 iT2

instance Show Isect where
    show (Isect t n _ _) = "time: " ++ show t ++ ", normal: " ++ show n

class At a where
    at :: a -> Ray -> Vector Double

instance At Double where
    at t (Ray p d) = p + (fromList [t] :: Vector Double) * d

instance At Isect where 
    at (Isect t _ _ _) (Ray p d) = p + (fromList [t] :: Vector Double) * d

type Color = Vector Double

data MatParam = MatParam Color
    deriving (Show, Read, Eq)

data Material = Material {ka :: MatParam,
                          ks :: MatParam,
                          kd :: MatParam,
                          ke :: MatParam,
                          kr :: MatParam,
                          kt :: MatParam,
                          shininess :: Double}
    deriving (Show, Read, Eq)

data Light =    PointLight       {lColor :: Color,
                                  lPosition :: Vector Double,
                                  constantTerm :: Double,
                                  linearTerm :: Double,
                                  quadraticTerm :: Double}
            |   DirectionalLight {lColor :: Color,
                                  lOrientation :: Vector Double}
    deriving (Show, Read, Eq)

type Lights = [Light]

lightColor :: Light -> Color
lightColor (PointLight c _ _ _ _) = c
lightColor (DirectionalLight c _) = c

getDirection :: Light -> Vector Double -> Vector Double
getDirection (PointLight _ pos _ _ _) p = normalize (pos - p)
getDirection (DirectionalLight _ o) _ = -o

distanceAttenuation :: Light -> Vector Double -> Double
distanceAttenuation (PointLight _ pos c b a) p = min 1.0 atten
    where atten = (c * dist * dist + b * dist + a) ** (-1.0)
          dist = norm_2 (p - pos)
distanceAttenuation (DirectionalLight _ _) p = 1.0

shadowAttenuation :: Scene -> Light -> Vector Double -> Double
shadowAttenuation scene (PointLight _ pos _ _ _) p = 
    let shadowDir = normalize (pos - p) in
    let ray = Ray p shadowDir in
    let isect = intersect scene ray in
    case isect of
        Nothing -> 1
        Just (Isect t _ _ _) ->
            if (t < 0) || (t > norm_2 (pos - p)) then
                1
            else
                0
shadowAttenuation scene dl@(DirectionalLight _ _) p =
    let shadowDir = normalize (getDirection dl p) in
    let ray = Ray p shadowDir in
    let isect = intersect scene ray in
    case isect of
        Nothing -> 1
        Just (Isect t _ _ _) ->
            if t < 0 then
                1
            else
                0

data Geometry = Sphere {sphereCenter :: Vector Double,
                        sphereRadius :: Double,
                        sphereMaterial :: Material}
    deriving (Show, Read, Eq)

type Objects = [Geometry]

data Scene = Scene {scLights :: Lights,
                    scObjects :: Objects,
                    scAmbient :: Color}
    deriving (Show, Read, Eq)

data Camera = Camera {eye :: Vector Double,
                      u :: Vector Double,
                      v :: Vector Double,
                      look :: Vector Double}
    deriving (Show, Read, Eq)

getCamera :: Vector Double -> Vector Double -> Vector Double -> Double -> Double -> Camera
getCamera position viewDir upDir aspect fov =
    let z = -viewDir in
    let y = upDir in
    let x = cross y z in
    let m = fromRows [x,y,z] in
    let fov' = fov * pi / 180.0 in
    let normalizedHeight = 2 * tan (fov' / 2.0) in
    let u = (flatten (m <> (fromColumns [(fromList [1,0,0])]))) * (fromList [normalizedHeight * aspect]) in
    let v = (flatten (m <> (fromColumns [(fromList [0,1,0])]))) * (fromList [normalizedHeight]) in
    let look = (flatten (m <> (fromColumns [(fromList [0,0,-1])]))) in
    Camera position u v look

rayThrough :: Camera -> Double -> Double -> Ray
rayThrough (Camera eye u v look) x y = Ray eye dir
    where dir = normalize (look + x' * u + y' * v)
          x' = fromList [x - 0.5]
          y' = fromList [y - 0.5]

intersectObj :: Ray -> Geometry -> Maybe Isect
intersectObj r@(Ray rayPos rayDir) s@(Sphere center radius material) =
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
            pure (Isect t1 (normalize (at t1 r)) s material)
        else
            pure (Isect t2 (normalize (at t2 r)) s material)

intersect :: Scene -> Ray -> Maybe Isect
intersect scene@(Scene _ objects _) r =
    let isects = filter (/= Nothing) . map (intersectObj r) $ objects in
    if length isects == 0 then
        Nothing
    else
        minimum isects

perLightDiffuse :: Light -> Vector Double -> Vector Double -> Vector Double -> Double
perLightDiffuse light isectPoint rayDir normal = 
    let lightDir = getDirection light isectPoint in 
    let ddot = lightDir <.> (if (rayDir <.> normal > 0) then -normal else normal) in
    let dmax = max ddot 0.0 in
    dmax

perLightSpecular :: Light -> Vector Double -> Vector Double -> Vector Double -> Double
perLightSpecular light isectPoint rayDir normal =
    let lightDir = getDirection light isectPoint in 
    let reflect = lightDir - (fromList [2 * (lightDir <.> normal)]) * normal in
    if norm_2 reflect == 0 then
        0
    else
        let reflectNorm = normalize reflect in
        let sdot = rayDir <.> reflect in
        let smax = max sdot 0.0 in
        smax

combineLightTerms :: Scene -> Vector Double -> Vector Double -> Vector Double -> Double -> Light -> Color
combineLightTerms scene isectPoint rayDir normal alpha light =
    let diffuse = perLightDiffuse light isectPoint rayDir normal in
    let specular = perLightSpecular light isectPoint rayDir normal in
    let color = lightColor light in
    let atten = (distanceAttenuation light isectPoint) * (shadowAttenuation scene light isectPoint) in
    color * (fromList [(diffuse + specular) * atten])

shade :: Scene -> Ray -> Isect -> Color
shade scene@(Scene lights objects ambient) r@(Ray p d) i@(Isect t normal _ material) = 
    let (Material kaM ksM kdM keM krM ktM shiny) = material in
    let (MatParam ke) = keM in
    let emissive = ke in
    let (MatParam ka) = kaM in
    let amb = ka * ambient in
    let isectPoint = at t r in
    let alpha = shiny in
    emissive + amb + (sum . map (combineLightTerms scene isectPoint d normal alpha) $ lights)

data Screen = Screen {screenWidth :: Int,
                      screenHeight :: Int}
    deriving (Show, Read, Eq)

clamp :: Double -> Double -> Vector Double -> Vector Double
clamp a b = fromList . map (\x -> max a (min b x)) . toList

trace :: Scene -> Camera -> Double -> Double -> Color
trace scene camera x y =
    let ray = rayThrough camera x y in
    let color = traceRay scene ray 0 0 in
    clamp 0 1 color

tracePixel :: Scene -> Screen -> Camera -> Int -> Int -> Color
tracePixel scene (Screen width height) camera i j =
    let x = (fromIntegral i :: Double) / (fromIntegral width :: Double) in
    let y = (fromIntegral j :: Double) / (fromIntegral height :: Double) in
    let color = trace scene camera x y in 
    (fromList [255.0]) * color

traceRay :: Scene -> Ray -> Int -> Double -> Color
traceRay scene ray@(Ray p d) depth t =
    let mIsect = intersect scene ray in
    case mIsect of
        Nothing -> fromList[0,0,0]
        (Just isect) -> shade scene ray isect

type Image = ([[Color]], Int, Int)

traceImage :: Scene -> Screen -> Camera -> Image
traceImage scene screen@(Screen width height) camera =
    let (w, h) = (width - 1, height - 1) in
    let img = [[tracePixel scene screen camera i j | j <- [0..h]] | i <- [0..w]] in
    (img, width, height)

rgbify :: [Double] -> G.Pixel G.RGB Double
rgbify vec = G.PixelRGB (r / 255.0) (g / 255.0) (b / 255.0)
    where r = vec !! 0
          g = vec !! 1
          b = vec !! 2

render :: Image -> G.Image G.VU G.RGB Double
render (image, width, height) = G.fromLists . format $ image
    where format = map (map rgbify) . map (map toList)

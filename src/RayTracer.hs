module RayTracer where

import RayLib
import Numeric.LinearAlgebra
import qualified Graphics.Image as G

data Screen = Screen {screenWidth :: Int,
                      screenHeight :: Int}

trace :: Scene -> Camera -> Double -> Double -> Color
trace scene camera x y =
    let ray = rayThrough camera x y in
    fromList [0,0,0]

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

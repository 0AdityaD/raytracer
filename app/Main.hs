module Main where

import RayLib
import RayTracer
import Numeric.LinearAlgebra
import qualified Graphics.Image as G

main :: IO ()
main = do let screen = Screen 256 256
          let ka = \x -> (fromList [0,1,1]) 
          let ks = \x -> (fromList [1,1,0])
          let kd = \x -> (fromList [0.4,0.4,0])
          let ke = \x -> (fromList [0,0,0])
          let kr = \x -> (fromList [0,0,0])
          let kt = \x -> (fromList [0,0,0])
          let shininess = \x -> 64
          let material = Material ka ks kd ke kr kt shininess
          let sphere1 = Sphere (fromList [-5.0,0.0,-5.0]) 5.0 material
          let sphere2 = Sphere (fromList [-5.0,-5.0,0.0]) 5.0 material
          let sphere3 = Sphere (fromList [0.0,-5.0,-5.0]) 5.0 material
          let scene = Scene [] [sphere1] (fromList [1,0,0.1])
          let image = traceImage scene screen camera
          let toWrite = render image
          G.writeImage "test.png" toWrite

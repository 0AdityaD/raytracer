module Main where

import RayLib
import Numeric.LinearAlgebra
import qualified Graphics.Image as G

main :: IO ()
main = do let screen = Screen 256 256
          let ka = MatParam (fromList [0,1,1])
          let ks = MatParam (fromList [1,1,0])
          let kd = MatParam (fromList [0.4,0.4,0])
          let ke = MatParam (fromList [0,0,0])
          let kr = MatParam (fromList [0,0,0])
          let kt = MatParam (fromList [0,0,0])
          let shininess = 64
          let material = Material ka ks kd ke kr kt shininess
          let sphere1 = Sphere (fromList [0,0,0]) 1.5 material
          let scene = Scene [] [sphere1] (fromList [1,0,0.1])
          putStrLn . show $ scene
          let camera = getCamera (fromList [0,0,-4]) (fromList [0,0,1]) (fromList [0,1,0]) 1 90
          putStrLn . show $ camera
          putStrLn . show $ screen
          let image = traceImage scene screen camera
          let toWrite = render image
          G.writeImage "test.png" toWrite

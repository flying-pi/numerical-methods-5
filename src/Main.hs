module Main where

import Prelude

import Methods

import SpecialAlg

a = 3.0
b = 8.0

-- a = -1.0
-- b = 1.0
fun x= sin (exp (x/3) + x)

main :: IO ()
main = do
  putStrLn "enter count iteration :: "
  iteerationCount <- getLine
  putStrLn ("left rectangle :: \t\t"++(show$rectangleLeft a b (read iteerationCount :: Double) fun))
  putStrLn ("right rectangle :: \t\t"++(show$rectangleRight a b (read iteerationCount :: Double) fun))
  putStrLn ("center rectangle :: \t\t"++(show$rectangleCenter a b (read iteerationCount :: Double) fun))
  putStrLn ("trapeze :: \t\t\t"++(show$trapeze a b (read iteerationCount :: Double) fun))
  putStrLn ("parabola :: \t\t\t"++(show$parabola a b (read iteerationCount :: Double) fun))
  putStrLn ("newton :: \t\t\t"++(show$((newton a b 4 fun))))
  putStrLn ("newton :: \t\t\t"++(show$newton a b 5 fun))
  putStrLn ("hause :: \t\t\t"++(show$hause a b 4 fun))
  putStrLn ("hause :: \t\t\t"++(show$hause a b 5 fun))
  putStrLn ("hause :: \t\t\t"++(show$hause a b 6 fun))
  putStrLn ("chebushov :: \t\t\t"++(show$chebushov a b 4 fun))
  putStrLn ("chebushov :: \t\t\t"++(show$chebushov a b 5 fun))
  putStrLn ("chebushov :: \t\t\t"++(show$chebushov a b 6 fun))


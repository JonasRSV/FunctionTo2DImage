module Main where

import Codec.BMP
import System.Environment
import Data.ByteString
import Data.Word8
import System.Random


data RGBA = RGBA Word8 Word8 Word8 Word8
data Mathy = FALSE | TRUE | BORDER

rgbas2words :: [RGBA] -> [Word8]
rgbas2words [] = []
rgbas2words (RGBA red green blue alpha:colors) = red : green : blue : alpha : rgbas2words colors

num2color :: Integral a => a -> RGBA
num2color v = let red = v `quot` 256
                  green = red `quot` 256
                  blue = red `quot` 256
                  alpha = red `quot` 256
                in RGBA (sq red) (sq green) (sq blue) (sq alpha)
  where
    sq = fromIntegral . (`mod` 256) 

randomColor :: IO RGBA
randomColor = do
                v <- randomIO :: IO Int
                return $ num2color v

defaultImageDimensions :: (Int, Int)
defaultImageDimensions = (1440, 900)

defaultBG :: RGBA
defaultBG = RGBA 255 255 255 255

translate2D :: (Int, Int) -> Int -> (Int, Int)
translate2D (width, heigth) position = (xmid - position `mod` width, ymid - position `quot` width)
  where 
    xmid = width `quot` 2
    ymid = heigth `quot` 2

setImage :: (Int, Int) -> (Int -> Int -> Mathy) -> RGBA -> RGBA -> [RGBA]
setImage dim@(width, heigth) c t b = Prelude.take (width * heigth) lazyImage
  where
    lazyBG :: [(RGBA, Int)]
    lazyBG = Prelude.zip (repeat defaultBG) [0..]
    lazyImage :: [RGBA]
    lazyImage = Prelude.map (\(color, index) -> 
                                    case uncurry c $ translate2D dim index of
                                      FALSE -> color
                                      TRUE -> t
                                      BORDER -> b ) lazyBG



testImage :: Int -> Int -> Mathy
testImage x y
  |  y * y + x * x < 100000 = TRUE
  |  y * y + x * x > 100010 = FALSE
  | otherwise = BORDER

main :: IO ()
main = 
  do 
    (fileName:_) <- getArgs
    t <- randomColor
    b <- randomColor
    let (x, y) = defaultImageDimensions
    let rgba   = pack . rgbas2words $ setImage defaultImageDimensions testImage t b 
    let bmp    = packRGBA32ToBMP x y rgba
    writeBMP fileName bmp
    

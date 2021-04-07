-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

--import Figures
import Arithmics
import Codec.Picture
import Codec.Picture.Types
import System.Environment

width :: Int
width = 800

height :: Int
height = 600

generateTrace :: DynamicImage
--generateTrace = ImageRGB8 (generateImage simple2D width height)
--generateTrace = ImageRGB8 (generateImage colors2D width height)
generateTrace = ImageRGB8 (generateImage sphere width height)

simple2D :: Int -> Int -> PixelRGB8
simple2D px py =
  if -1 < px && px < 800 && 250 < py && py < 600 then
    PixelRGB8 255 128 128
  else
    PixelRGB8 128 128 255

colors2D :: Int -> Int -> PixelRGB8
colors2D px py =
  --Red
  if 300 < px && px < 500 && 200 < py && py < 400
    then PixelRGB8 255 128 128
  --Yellow
  else if 400 < px && px < 600 && 300 < py && py < 500
    then PixelRGB8 255 255 0
  --Orange
  else if 200 < px && px < 400 && 100 < py && py < 300
    then PixelRGB8 255 153 51
  --Blue
  else if 400 < px && px < 600 && 100 < py && py < 300
      then PixelRGB8 51 153 255
  --Purple
  else if 200 < px && px < 400 && 300 < py && py < 500
        then PixelRGB8 153 51 255

  --Green mix
  else if 500 < px && px < 700 && 200 < py && py < 400
        then PixelRGB8 0 204 0
  --Green-grey mix
  else if 300 < px && px < 500 && 25 < py && py < 125
        then PixelRGB8 128 153 128
  --Pink mix
    else if 100 < px && px < 300 && 200 < py && py < 400
          then PixelRGB8 199 97 140
  --Creme mix
    else if 300 < px && px < 500 && 400 < py && py < 600
          then PixelRGB8 204 153 128

  else PixelRGB8 194 214 214


sphere :: Int -> Int -> PixelRGB8
sphere px py =
  case reflectedColor3D line of
      SkyColor -> PixelRGB8 200 230 255
      DarkColor -> PixelRGB8 82 82 122
      BrightColor -> PixelRGB8 255 255 240
    where
      x = ((fromIntegral px) - 550.0)/200.0
      y = (300.0 - (fromIntegral py))/200.0
      horisonten = lineFrom (Vector3D 0 1 0) (Vector3D x 3 y)
      line = reflectedLine3D sphere horisonten

main :: IO ()
main = do
  putStrLn "Creating image ..."
  savePngImage "sphere.png" generateTrace
  putStrLn "Done!"

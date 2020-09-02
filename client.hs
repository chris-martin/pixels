import Relude
import Control.Exception (bracket)
import Data.Bits (shift, (.|.))
import qualified Data.Time as Time
import SharedMemory (openSharedMemory)
import System.Posix.IO (closeFd)
import System.Posix.SharedMem (ShmOpenFlags (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import Foreign.ForeignPtr (finalizeForeignPtr, withForeignPtr)

width, height :: Num a => a
width = 800
height = 500

main :: IO ()
main =
    Time.getCurrentTime >>= \t1 ->
    pure (Time.diffTimeToPicoseconds $ Time.utctDayTime t1) >>= \seed ->
    bracket (openSharedMemory "pixels" (width * height * 4) (ShmOpenFlags True False False True) 0) (\(x, fd) -> finalizeForeignPtr x <> closeFd fd) \(sharedImageDataForeignPtr, _) ->
    withForeignPtr sharedImageDataForeignPtr \imageData' ->
    pure (castPtr imageData') >>= \imageData ->
    write imageData (circle seed) *>
    Time.getCurrentTime >>= \t2 ->
    print (Time.diffUTCTime t2 t1)

type Image = Int -> Int -> Word64

type Seed = Integer

square :: Int -> Int
square x = x * x

circle :: Seed -> Image
circle seed = \x y ->
    let
      dX = square (x - x2)
      dY = square (y - y2)
      s = dX + dY
    in
      if x > x2 - 65 &&
         x < x2 + 65 &&
         y > y2 - 65 &&
         y < y2 + 65 &&
         s > square 60 &&
         s < square 65
      then c1 else c2
  where
    c1 = rgb 150 0 150
    c2 = rgb 255 180 (fromInteger (seed `mod` 255))
    x2 = width `div` 2
    y2 = height `div` 2

write :: Ptr Word64 -> Image -> IO ()
write p f =
  traverse_ (\(i, c) -> pokeElemOff p i c) $
    zipWith
      (\(y, x) i -> (i, f (x + 1) y `shift` 32 .|. f x y))
      ((,) <$> [0 .. height - 1] <*> [0, 2 .. width - 1])
      [0 ..]

rgb :: Word64 -> Word64 -> Word64 -> Word64
rgb r g b = shift r 16 .|. shift g 8 .|. b :: Word64

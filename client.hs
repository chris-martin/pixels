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

main :: IO ()
main =
    Time.getCurrentTime >>= \t1 ->
    pure (Time.diffTimeToPicoseconds $ Time.utctDayTime t1) >>= \seed ->
    bracket (openSharedMemory "pixels" (500 * 500 * 4) (ShmOpenFlags True False False True) 0) (\(x, fd) -> finalizeForeignPtr x <> closeFd fd) \(sharedImageDataForeignPtr, _) ->
    withForeignPtr sharedImageDataForeignPtr \imageData' ->
    pure (castPtr imageData') >>= \imageData ->
    write imageData (circle seed) *>
    Time.getCurrentTime >>= \t2 ->
    print (Time.diffUTCTime t2 t1)

type Image = Int -> Int -> Word32

type Seed = Integer

square :: Int -> Int
square x = x * x

circle :: Seed -> Image
circle seed = \x y ->
    let
      dX = square (x - 250)
      dY = square (y - 250)
      s = dX + dY
    in
      if s > 3600 && s < 4225 then c1 else c2
  where
    c1 = rgb 150 0 150
    c2 = rgb 255 180 (fromInteger (seed `mod` 255))

write :: Ptr Word32 -> Image -> IO ()
write p f =
  traverse_ (\(i, c) -> pokeElemOff p i c) $
    zipWith
      (\(y, x) i -> (i, f x y))
      ((,) <$> [0..500] <*> [0..499])
      [0..]

rgb :: Word32 -> Word32 -> Word32 -> Word32
rgb r g b = shift r 16 .|. shift g 8 .|. b :: Word32

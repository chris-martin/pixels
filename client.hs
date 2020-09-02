import Relude
import Control.Exception (bracket)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Array (copyArray)
import Data.Bits (shift, (.|.))
import qualified Data.Time as Time
import SharedMemory (openSharedMemory)
import System.Posix.IO (closeFd)
import System.Posix.SharedMem (ShmOpenFlags (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (finalizeForeignPtr, withForeignPtr)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BSB

main :: IO ()
main =
    Time.getCurrentTime >>= \t1 ->
    pure (Time.diffTimeToPicoseconds $ Time.utctDayTime t1) >>= \seed ->
    bracket (openSharedMemory "pixels" (500 * 500 * 4) (ShmOpenFlags True False False True) 0) (\(x, fd) -> finalizeForeignPtr x <> closeFd fd) \(sharedImageDataForeignPtr, _) ->
    withForeignPtr sharedImageDataForeignPtr \imageData' ->
    pure (castPtr imageData' :: Ptr CChar) >>= \imageData ->
    unsafeUseAsCString (LBS.toStrict (lbs seed)) (\p -> copyArray imageData p (500 * 500 * 4)) *>
    Time.getCurrentTime >>= \t2 ->
    print (Time.diffUTCTime t2 t1)

square :: Int -> Int
square x = x * x

lbs :: Integer -> LBS.ByteString
lbs seed = BSB.toLazyByteStringWith (BSB.untrimmedStrategy (500 * 500 * 4) (500 * 500 * 4)) mempty bsb
  where
    c1 = rgb 150 0 150
    c2 = rgb 255 180 (fromInteger (seed `mod` 255))

    bsb = foldMap (\y -> foldMap (\x -> BSB.word32LE (circle c1 c2 x y)) [0 .. 499]) [0 .. 499]

rgb :: Word32 -> Word32 -> Word32 -> Word32
rgb r g b = shift r 16 .|. shift g 8 .|. b :: Word32

circle :: Word32 -> Word32 -> Int -> Int -> Word32
circle c1 c2 x y = if s > 3600 && s < 4225 then c1 else c2
  where
    dX = square (x - 250)
    dY = square (y - 250)
    s = dX + dY

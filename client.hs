import Relude
import Control.Exception (bracket)
import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X
import Foreign.C.Types (CChar, CInt)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Storable
import Data.Bits
import Data.Time (getCurrentTime, diffUTCTime)
import SharedMemory
import System.Posix.IO (closeFd)
import System.Posix.SharedMem
import System.Posix.Types
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr

main =
    getCurrentTime >>= \t1 ->
    bracket (openSharedMemory "pixels" (500 * 500 * 4) (ShmOpenFlags True False False True) 0) (\(x, fd) -> finalizeForeignPtr x <> closeFd fd) \(sharedImageDataForeignPtr, _) ->
    withForeignPtr sharedImageDataForeignPtr \imageData' ->
    pure (castPtr imageData' :: Ptr Word32) >>= \imageData ->
    let
      c1 = rgb 150 0 150
      c2 = rgb 255 180 255
      rgb r g b = shift r 16 .|. shift g 8 .|. b
    in
      traverse_
          (\(x, y) ->
            let
              dX = square (x - 250)
              dY = square (y - 250)
              s = dX + dY
              c = if s > 3600 && s < 4225 then c1 else c2
            in
              pokeElemOff imageData (y * 500 + x) c
          )       ((,) <$> [0..499] <*> [0..499])
    *>
    getCurrentTime >>= \t2 ->
    print (diffUTCTime t2 t1)

square x = x * x

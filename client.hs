import Relude
import Control.Exception (bracket)
import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X
import Foreign.C.Types (CChar, CInt)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Storable
import Data.Bits
import SharedMemory
import System.Posix.IO (closeFd)
import System.Posix.SharedMem
import System.Posix.Types
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr

main =
    bracket (openSharedMemory "pixels" (500 * 500 * 4) (ShmOpenFlags True False False True) 0) (\(x, fd) -> finalizeForeignPtr x <> closeFd fd) \(sharedImageDataForeignPtr, _) ->
    withForeignPtr sharedImageDataForeignPtr \imageData' ->
    pure (castPtr imageData' :: Ptr CChar) >>= \imageData ->
    let
      c1 = (150, 0, 150)
      c2 = (255, 180, 255)
    in
      traverse_
          (\(x, y) ->
            let
              dX = (fromIntegral x - 250 :: Double) ^ (2 :: Int)
              dY = (fromIntegral y - 250 :: Double) ^ (2 :: Int)
              s = dX + dY
              (r, g, b) = if s > 3600 && s < 4225 then c1 else c2
            in
              pokeElemOff imageData (4 * (y * 500 + x) + 0) r *>
              pokeElemOff imageData (4 * (y * 500 + x) + 1) g *>
              pokeElemOff imageData (4 * (y * 500 + x) + 2) b
          )
          ((,) <$> [0..499] <*> [0..499])

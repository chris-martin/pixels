module Pixels.Server.Main where

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

width, height :: Num a => a
width = 800
height = 500

main :: IO ()
main = withSetup eventLoop

-- everything that gets created before the main event loop starts
data Setup = Setup
  { atoms :: {-# UNPACK #-} !Atoms
  , display :: !X.Display
  , window :: !X.Window
  , eventPtr :: !X.XEventPtr
  , image :: !X.Image
  , gc :: !X.GC
  }

withSetup :: (Setup -> IO a) -> IO a
withSetup go =
    withDefaultDisplay \display ->
    createAtoms display >>= \atoms ->
    pure (X.defaultScreenOfDisplay display) >>= \screen ->
    pure (X.defaultVisualOfScreen screen) >>= \visual ->
    withWindow display \window ->
    bracket (X.createGC display window) (X.freeGC display) \gc ->

    -- background
    bracket (X.createPixmap display window 2 2 depth) (X.freePixmap display) \bg ->
    X.setForeground display gc (shift 150 16 .|. shift 0 8 .|. 150) *>
    X.drawPoints display bg gc [X.Point 0 1, X.Point 1 0] X.coordModeOrigin *>
    X.setForeground display gc (shift 255 16 .|. shift 180 8 .|. 255) *>
    X.drawPoints display bg gc [X.Point 0 0, X.Point 1 1] X.coordModeOrigin *>
    X.setWindowBackgroundPixmap display window bg *>

    -- initialize image
    -- don't need this anymore because openSharedMemory is allocating it instead
    -- callocBytes (width height * 4) >>= \imageData ->

    bracket (openSharedMemory "pixels" (width * height * 4) (ShmOpenFlags True True True True) (ownerRead .|. ownerWrite)) (\(x, fd) -> shmUnlink "pixels") \(sharedImageDataForeignPtr, _) ->
    withForeignPtr sharedImageDataForeignPtr \imageData' ->
    pure (castPtr imageData' :: Ptr CChar) >>= \imageData ->

    --bracket (X.createImage display visual depth X.zPixmap 0 imageData width height 8 0) X.destroyImage \image ->
    --todo: when we destroy the image, X frees the memory, and that's not what we want
    X.createImage display visual depth X.zPixmap 0 imageData width height 8 0 >>= \image ->

    X.allocaXEvent \eventPtr ->
    pure Setup{ atoms, display, window, gc, image, eventPtr } >>= \setup ->
    setWMProtocols setup *>
    X.mapWindow display window *>

    go setup

-- https://jameshfisher.com/2017/02/24/what-is-mode_t/
ownerRead, ownerWrite :: CMode
ownerRead = 1 `shift` 8
ownerWrite = 1 `shift` 7

eventLoop :: Setup -> IO ()
eventLoop setup@Setup{ display, eventPtr } =
    fix \loop ->
    X.nextEvent display eventPtr *>
    X.getEvent eventPtr >>= \event ->
    if | isQuitEvent setup event -> pure ()
       | isExposeEvent event -> putImage setup *> loop
       | otherwise -> loop

putImage :: Setup -> IO ()
putImage Setup { display, window, gc, image } = X.putImage display window gc image 0 0 0 0 width height

isExposeEvent :: X.Event -> Bool
isExposeEvent = \case X.ExposeEvent{} -> True; _ -> False

withDefaultDisplay :: (X.Display -> IO a) -> IO a
withDefaultDisplay = withDisplay ""

withDisplay :: String -> (X.Display -> IO a) -> IO a
withDisplay displayName = bracket (X.openDisplay displayName) X.closeDisplay

withWindow :: X.Display -> (X.Window -> IO a) -> IO a
withWindow display = bracket (createWindow display) (X.destroyWindow display)

createWindow :: X.Display -> IO X.Window
createWindow display =
    X.allocaSetWindowAttributes \attr ->
    X.set_event_mask attr X.exposureMask *> -- requires cwEventMask below
    pure (X.defaultRootWindow display) >>= \parent ->
    pure (X.defaultScreenOfDisplay display) >>= \screen ->
    pure (X.defaultVisualOfScreen screen) >>= \visual ->
    X.createWindow display parent
        0 0 -- position
        width height
        0 -- border width
        depth
        X.inputOutput
        visual
        X.cWEventMask -- which window attributes are defined in attr
        attr

setWMProtocols :: Setup -> IO ()
setWMProtocols Setup{ atoms = Atoms{..}, display, window } =
    X.setWMProtocols display window [atom_WM_DELETE_WINDOW]

isQuitEvent :: Setup -> X.Event -> Bool
isQuitEvent
    Setup{ atoms = Atoms{ atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW } }
    X.ClientMessageEvent{ ev_message_type = x, ev_data = listToMaybe -> Just y } =
        x == atom_WM_PROTOCOLS && y `integerEq` atom_WM_DELETE_WINDOW
isQuitEvent _ _ = False

-- equality test between two different types of integers
integerEq :: (Integral a, Integral b) => a -> b -> Bool
integerEq a b = toInteger a == toInteger b

-- all of the X atoms that we use in the program
data Atoms = Atoms
  { atom_WM_DELETE_WINDOW :: !X.Atom
  , atom_WM_PROTOCOLS :: !X.Atom
  }

createAtoms :: X.Display -> IO Atoms
createAtoms display =
    atom "WM_DELETE_WINDOW" >>= \atom_WM_DELETE_WINDOW ->
    atom "WM_PROTOCOLS" >>= \atom_WM_PROTOCOLS ->
    pure Atoms{..}
  where
    atom string = X.internAtom display string False

type Depth = CInt

depth :: Depth
depth = 24

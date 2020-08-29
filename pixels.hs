module Main (main) where

import Relude
import Control.Exception (bracket)
import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X

-- Everything that gets created before the main event loop starts.
data Setup = Setup { atoms :: Atoms, display :: X.Display, window :: X.Window, eventPtr :: X.XEventPtr }

-- All of the X atoms that we use in the program.
data Atoms = Atoms { atom_WM_DELETE_WINDOW :: X.Atom, atom_WM_PROTOCOLS :: X.Atom }

main :: IO ()
main = withSetup eventLoop

withSetup :: (Setup -> IO a) -> IO a
withSetup f =
    withDefaultDisplay \display ->
    createAtoms display >>= \atoms ->
    withWindow display \window ->
    X.allocaXEvent \eventPtr ->
    Setup{ atoms, display, window, eventPtr } & \setup ->
    setWMProtocols setup *>
    X.mapWindow display window *>
    f setup

eventLoop :: Setup -> IO ()
eventLoop Setup{ display, eventPtr, atoms } =
    fix \loop ->
    X.nextEvent display eventPtr *>
    X.getEvent eventPtr >>= \event ->
    if | isQuitEvent atoms event -> pure ()
       | otherwise -> loop

withDefaultDisplay :: (X.Display -> IO a) -> IO a
withDefaultDisplay = withDisplay ""

withDisplay :: String -> (X.Display -> IO a) -> IO a
withDisplay displayName = bracket (X.openDisplay displayName) X.closeDisplay

withWindow :: X.Display -> (X.Window -> IO a) -> IO a
withWindow display = bracket (createWindow display) (X.destroyWindow display)

createWindow :: X.Display -> IO X.Window
createWindow display =
    X.allocaSetWindowAttributes \attr ->
    X.defaultRootWindow display & \parent ->
    defaultVisualOfDisplay display & \visual ->
    X.createWindow display parent 0 0 500 500 5 0 X.copyFromParent visual 0 attr

defaultVisualOfDisplay :: X.Display -> X.Visual
defaultVisualOfDisplay = X.defaultVisualOfScreen . X.defaultScreenOfDisplay

setWMProtocols :: Setup -> IO ()
setWMProtocols Setup{ atoms = Atoms{..}, display, window } =
    X.setWMProtocols display window [atom_WM_DELETE_WINDOW]

isQuitEvent :: Atoms -> X.Event -> Bool
isQuitEvent
    Atoms{ atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW }
    X.ClientMessageEvent{ ev_message_type, ev_data } =
        ev_message_type == atom_WM_PROTOCOLS &&
        fmap toInteger (listToMaybe ev_data) == Just (toInteger atom_WM_DELETE_WINDOW)
isQuitEvent _ _ = False

createAtoms :: X.Display -> IO Atoms
createAtoms display =
  do
    atom_WM_DELETE_WINDOW <- atom "WM_DELETE_WINDOW"
    atom_WM_PROTOCOLS <- atom "WM_PROTOCOLS"
    pure Atoms{..}
  where
    atom string = X.internAtom display string False

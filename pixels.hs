import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Graphics.X11 as X
import qualified Graphics.X11.Xlib.Extras as X

main = withSetup eventLoop

withSetup a = withDefaultDisplay \display -> createAtoms display >>= \atoms -> withWindow display \window -> setWMProtocols atoms display window *> X.mapWindow display window *> a atoms display

eventLoop atoms display = X.allocaXEvent \eventPtr -> runMaybeT $ forever $ liftIO (X.nextEvent display eventPtr) *> liftIO (X.getEvent eventPtr) >>= handleEvent atoms display

withDefaultDisplay = withDisplay ""

withDisplay displayName = bracket (X.openDisplay displayName) X.closeDisplay

withWindow display = bracket (createWindow display) (X.destroyWindow display)

createWindow display = X.allocaSetWindowAttributes \attr -> X.createWindow display (X.defaultRootWindow display) 0 0 500 500 5 0 X.copyFromParent (defaultVisualOfDisplay display) 0 attr

defaultVisualOfDisplay display = X.defaultVisualOfScreen (X.defaultScreenOfDisplay display)

setWMProtocols Atoms{..} display window = X.setWMProtocols display window [wmDeleteWindow]

handleEvent Atoms{..} display = \case
    X.ClientMessageEvent{ev_message_type, ev_data} | ev_message_type == wmProtocols, fmap toInteger (headMaybe ev_data) == Just (toInteger wmDeleteWindow) -> exitMaybeT
    _ -> pure ()

headMaybe = \case x : _ -> Just x; [] -> Nothing

exitMaybeT = MaybeT (pure Nothing)

data Atoms = Atoms { wmDeleteWindow :: X.Atom, wmProtocols :: X.Atom }

createAtoms display = let atom string = X.internAtom display string False in atom "WM_DELETE_WINDOW" >>= \wmDeleteWindow -> atom "WM_PROTOCOLS" >>= \wmProtocols -> pure Atoms{..}

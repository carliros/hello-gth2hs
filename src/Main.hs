module Main where
import Control.Monad ( void )
import Control.Monad.IO.Class ()
import Data.IORef ()
import Graphics.UI.Gtk
    ( AttrOp( (:=) )
    , widgetShowAll
    , initGUI
    , mainGUI
    , set
    , windowNew
    , windowTitle
    , windowDefaultWidth
    , windowDefaultHeight
    , windowResizable
    )

main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle          := "Calculator"
             , windowDefaultWidth   := 230
             , windowDefaultHeight  := 250
             --, windowResizable      := False --makes the windows not to be shown
             ]
  widgetShowAll window
  mainGUI

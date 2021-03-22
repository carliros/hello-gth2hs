module Main where
import Control.Monad ( void )
import Control.Monad.IO.Class (liftIO)
import Data.IORef ()
import Graphics.UI.Gtk.Layout.Grid (gridAttach, gridNew, gridSetRowHomogeneous)
import Graphics.UI.Gtk.Types (Button, GridClass, WidgetClass, Entry)

import Graphics.UI.Gtk
    ( AttrOp( (:=) )
    , widgetShowAll
    , initGUI
    , mainGUI
    , set
    , on
    , windowNew, windowTitle, windowDefaultWidth, windowDefaultHeight, windowResizable
    , entryNew, entryEditable, entryXalign, entryText
    , buttonNew, buttonLabel, buttonActivated
    , containerAdd
    , deleteEvent
    , mainQuit
    )

mkBtn :: String -> Entry -> IO Button
mkBtn label display = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ 
    set display [ entryText := label ]
  return btn

attach :: (GridClass parent, WidgetClass child) => Int -> Int -> Int -> Int -> parent -> child -> IO ()
attach x y w h parent child = gridAttach parent child x y w h

createCalcLayout :: GridClass parent => parent -> IO ()
createCalcLayout grid = do
  display <- entryNew
  set display [ entryEditable := False 
              , entryXalign := 1
              , entryText := "0"
              ]
  attach 0 0 5 1 grid display
  mkBtn "MC" display >>= attach 0 1 1 1 grid
  mkBtn "MR" display >>= attach 1 1 1 1 grid
  mkBtn "MS" display >>= attach 2 1 1 1 grid
  mkBtn "M+" display >>= attach 3 1 1 1 grid
  mkBtn "M–" display >>= attach 4 1 1 1 grid
  mkBtn "←"  display >>= attach 0 2 1 1 grid
  mkBtn "CE" display >>= attach 1 2 1 1 grid
  mkBtn "C"  display >>= attach 2 2 1 1 grid
  mkBtn "±"  display >>= attach 3 2 1 1 grid
  mkBtn "√"  display >>= attach 4 2 1 1 grid
  mkBtn "7"  display >>= attach 0 3 1 1 grid
  mkBtn "8"  display >>= attach 1 3 1 1 grid
  mkBtn "9"  display >>= attach 2 3 1 1 grid
  mkBtn "÷"  display >>= attach 3 3 1 1 grid
  mkBtn "%"  display >>= attach 4 3 1 1 grid
  mkBtn "4"  display >>= attach 0 4 1 1 grid
  mkBtn "5"  display >>= attach 1 4 1 1 grid
  mkBtn "6"  display >>= attach 2 4 1 1 grid
  mkBtn "*"  display >>= attach 3 4 1 1 grid
  mkBtn "1/x" display >>= attach 4 4 1 1 grid
  mkBtn "1"  display >>= attach 0 5 1 1 grid
  mkBtn "2"  display >>= attach 1 5 1 1 grid
  mkBtn "3"  display >>= attach 2 5 1 1 grid
  mkBtn "–"  display >>= attach 3 5 1 1 grid
  mkBtn "="  display >>= attach 4 5 1 2 grid
  mkBtn "0"  display >>= attach 0 6 2 1 grid
  mkBtn "."  display >>= attach 2 6 1 1 grid
  mkBtn "+"  display >>= attach 3 6 1 1 grid

main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle          := "Calculator"
             , windowDefaultWidth   := 230
             , windowDefaultHeight  := 250
             , windowResizable      := False
             ]
  grid <- gridNew
  gridSetRowHomogeneous grid True
  createCalcLayout grid
  containerAdd window grid
  window `on` deleteEvent $ do
    liftIO mainQuit 
    return False
  widgetShowAll window
  mainGUI

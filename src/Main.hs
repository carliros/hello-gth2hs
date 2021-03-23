module Main where

import Control.Monad ( void )
import Control.Monad.IO.Class (liftIO)
import Data.IORef ( IORef, newIORef, atomicModifyIORef )
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

data Value = Value String (Maybe Action)
data Action = Addition        String
            | Subtraction     String
            | Multiplication  String
            | Division        String

mapAction :: (String -> String) -> Action -> Action
mapAction f (Addition       x) = Addition       (f x)
mapAction f (Subtraction    x) = Subtraction    (f x)
mapAction f (Multiplication x) = Multiplication (f x)
mapAction f (Division       x) = Division       (f x)

getActionArg :: Action -> String
getActionArg (Addition       x) = x
getActionArg (Subtraction    x) = x
getActionArg (Multiplication x) = x
getActionArg (Division       x) = x

renderValue :: Value -> String
renderValue (Value x action) = g x ++ f a ++ (if null y then "" else g y)
  where (a, y) = case action of
                    Nothing                   -> ("", "")
                    Just (Addition arg)       -> ("+", arg)
                    Just (Subtraction arg)    -> ("-", arg)
                    Just (Multiplication arg) -> ("*", arg)
                    Just (Division arg)       -> ("÷", arg)
        f "" = ""
        f l = " " ++ l ++ " "
        g "" = "0"
        g xs = reverse xs

updateDisplay :: Entry -> Value -> IO ()
updateDisplay display value = set display [ entryText := renderValue value ]

mkBtn :: IORef Value -> String -> Entry -> (Value -> Value) -> IO Button
mkBtn st label display mutateState = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
    updateDisplay display value
  return btn

attach :: (GridClass parent, WidgetClass child) => Int -> Int -> Int -> Int -> parent -> child -> IO ()
attach x y w h parent child = gridAttach parent child x y w h

enterDot :: Value -> Value
enterDot (Value x action)
  = let f xs = if '.' `elem` xs then xs else '.' : xs
    in case action of 
          Nothing -> Value (f x) Nothing
          Just a  -> Value x (Just $ mapAction f a)

enterDigit :: Char -> Value -> Value
enterDigit ch (Value x action) = 
  case action of
    Nothing -> Value (ch:x) Nothing
    Just a  -> Value x (Just $ mapAction (ch:) a)

backspace :: Value -> Value
backspace (Value x action) = 
  case action of
    Nothing -> Value (drop 1 x) Nothing
    Just a  -> Value x (Just $ mapAction (drop 1) a)

operator :: (String -> Action) -> Value -> Value
operator op value = 
  let (Value x action) = equals value
  in Value x $ Just $
        case action of
          Nothing -> op ""
          Just a  -> op (getActionArg a)

clearEntry :: Value -> Value
clearEntry (Value x action) = 
  case action of
    Nothing -> Value "" Nothing
    Just  a -> if null (getActionArg a)
               then Value "" Nothing
               else Value x (Just $ mapAction (const "") a)

clearAll :: Value -> Value
clearAll = const (Value "" Nothing)

equals :: Value -> Value
equals currentAction@(Value x action) = 
  case action of
    Nothing          -> currentAction
    Just nextAction  -> if null (getActionArg nextAction)
                        then currentAction
                        else evaluateNextAction nextAction
  where evaluateNextAction nextAction = 
          let arg1 = toDouble x
              arg2 = toDouble (getActionArg nextAction)
              result = reverse . show $ 
                          case nextAction of 
                            Addition       _ -> arg1 + arg2
                            Subtraction    _ -> arg1 - arg2
                            Multiplication _ -> arg1 * arg2
                            Division       _ -> arg1 / arg2
          in Value result Nothing

toDouble :: String -> Double
toDouble "" = 0
toDouble ('.':xs) = toDouble ('0':'.':xs)
toDouble xs = read (reverse xs)

createCalcLayout :: GridClass parent => parent -> IORef Value -> IO ()
createCalcLayout grid st = do
  display <- entryNew
  set display [ entryEditable := False 
              , entryXalign := 1
              , entryText := "0"
              ]
  attach 0 0 5 1 grid display
  mkBtn st "MC" display id >>= attach 0 1 1 1 grid
  mkBtn st "MR" display id >>= attach 1 1 1 1 grid
  mkBtn st "MS" display id >>= attach 2 1 1 1 grid
  mkBtn st "M+" display id >>= attach 3 1 1 1 grid
  mkBtn st "M–" display id >>= attach 4 1 1 1 grid
  mkBtn st "←"  display id >>= attach 0 2 1 1 grid
  mkBtn st "CE" display clearEntry >>= attach 1 2 1 1 grid
  mkBtn st "C"  display clearAll >>= attach 2 2 1 1 grid
  mkBtn st "±"  display id >>= attach 3 2 1 1 grid
  mkBtn st "√"  display id >>= attach 4 2 1 1 grid
  mkBtn st "7"  display (enterDigit '7') >>= attach 0 3 1 1 grid
  mkBtn st "8"  display (enterDigit '8') >>= attach 1 3 1 1 grid
  mkBtn st "9"  display (enterDigit '9') >>= attach 2 3 1 1 grid
  mkBtn st "÷"  display (operator Division) >>= attach 3 3 1 1 grid
  mkBtn st "%"  display id >>= attach 4 3 1 1 grid
  mkBtn st "4"  display (enterDigit '4') >>= attach 0 4 1 1 grid
  mkBtn st "5"  display (enterDigit '5') >>= attach 1 4 1 1 grid
  mkBtn st "6"  display (enterDigit '6') >>= attach 2 4 1 1 grid
  mkBtn st "*"  display (operator Multiplication) >>= attach 3 4 1 1 grid
  mkBtn st "1/x" display id >>= attach 4 4 1 1 grid
  mkBtn st "1"  display (enterDigit '1') >>= attach 0 5 1 1 grid
  mkBtn st "2"  display (enterDigit '2') >>= attach 1 5 1 1 grid
  mkBtn st "3"  display (enterDigit '3') >>= attach 2 5 1 1 grid
  mkBtn st "–"  display (operator Subtraction) >>= attach 3 5 1 1 grid
  mkBtn st "="  display equals >>= attach 4 5 1 2 grid
  mkBtn st "0"  display (enterDigit '0') >>= attach 0 6 2 1 grid
  mkBtn st "."  display enterDot >>= attach 2 6 1 1 grid
  mkBtn st "+"  display (operator Addition) >>= attach 3 6 1 1 grid

main :: IO ()
main = do
  st <- newIORef (Value "" Nothing)
  void initGUI
  window <- windowNew
  set window [ windowTitle          := "Calculator"
             , windowDefaultWidth   := 230
             , windowDefaultHeight  := 250
             , windowResizable      := False
             ]
  grid <- gridNew
  gridSetRowHomogeneous grid True
  createCalcLayout grid st
  containerAdd window grid
  window `on` deleteEvent $ do
    liftIO mainQuit 
    return False
  widgetShowAll window
  mainGUI

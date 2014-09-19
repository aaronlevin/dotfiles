import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

myFocusedBorderColor :: String
myFocusedBorderColor = "#DEBDFF"

myNonFocusedBorderColor :: String
myNonFocusedBorderColor = "#383838"

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((0, 0x1008FF11), spawn "amixer set Master 2-")
  , ((0, 0x1008FF13), spawn "amixer set Master 2+")
  , ((0, 0x1008FF12), spawn "amixer set Master toggle")
  ]
  ++
  [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,1,2] -- was [0..] *** change to match your screen order ***
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayout = onWorkspace "4:gimp" gimp $
  -- avoidStruts $ toggleLayouts (noBorders Full)
  -- (Full ||| tiled ||| mosaic 2 [3,2] ||| Mirror tiled)
  avoidStruts $ layoutHook defaultConfig
  where
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta = 2/100
    ratio = 1/2
    gimp = withIM (0.11) (Role "gimp-toolbox") $
           reflectHoriz $
           withIM (0.15) (Role "gimp-dock") Full


main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/aaron/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = "urxvt"
        , modMask = mod4Mask
        , borderWidth = 1
        , startupHook = setWMName "LG3D"
        -- , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor = myNonFocusedBorderColor
        , workspaces = ["1", "2", "3", "4:gimp", "5", "6", "7", "8", "9"]
        } `additionalKeys` myKeys

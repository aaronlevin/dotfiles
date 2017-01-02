import           System.Environment (getEnvironment)
import           XMonad
import           XMonad.Config.Gnome (gnomeConfig, gnomeRun)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.IM
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run             (safeSpawn)

myFocusedBorderColor :: String
myFocusedBorderColor = "#DEBDFF"

myNonFocusedBorderColor :: String
myNonFocusedBorderColor = "#383838"

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((0, 0x1008FF11), spawn "amixer set Master 2-")
  , ((0, 0x1008FF13), spawn "amixer set Master 2+")
  , ((0, 0x1008FF12), spawn "amixer set Master toggle")
  , ((mod4Mask, xK_p), gnomeRun)
  , ((mod4Mask .|. shiftMask, xK_q), spawn "gnome-session-save --kill")
  ]
  ++
  [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,1,2] -- was [0..] *** change to match your screen order ***
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayout = onWorkspace "4:gimp" gimp $
  -- avoidStruts $ toggleLayouts (noBorders Full)
  -- (Full ||| tiled ||| mosaic 2 [3,2] ||| Mirror tiled)
  avoidStruts $ layoutHook def
  where
    -- tiled = ResizableTall nmaster delta ratio []
    -- nmaster = 1
    -- delta = 2/100
    -- ratio = 1/2
    gimp = withIM 0.11 (Role "gimp-toolbox") $
           reflectHoriz $
           withIM 0.15 (Role "gimp-dock") Full

gnomeRegister :: MonadIO m => m ()
gnomeRegister = io $ do
  x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
  whenJust x $ \sessionId -> safeSpawn "dbus-send" [ "--session"
                                                   , "--print-reply=literal"
                                                   , "--dest=org.gnome.SessionManager"
                                                   , "/org/gnome/SessionManager"
                                                   , "org.gnome.SessionManager.RegisterClient"
                                                   , "string:xmonad"
                                                   , "string:" ++ sessionId
                                                   ]

main :: IO ()
main = do
  safeSpawn "setxkbmap" ["-option" , "caps:super" ]
  safeSpawn "xmodmap" [ "/home/aaronlevin/.Xmodmap" ]
  xmonad $ gnomeConfig
    { terminal = "/home/aaronlevin/.nix-profile/bin/urxvt"
    , modMask = mod4Mask
    , borderWidth = 1
    , startupHook = gnomeRegister >> setWMName "LG3D"
    , layoutHook = myLayout
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNonFocusedBorderColor
    , workspaces = ["1", "2", "3", "4:gimp", "5", "6", "7", "8", "9"]
    } `additionalKeys` myKeys

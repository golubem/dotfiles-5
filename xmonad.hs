-- beginning

import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import System.IO

myModMask      = mod4Mask
myTerminal     = "konsole"
myBorderWidth  = 5
myBorderColor  = "#363636"
myFocusedColor = "#b8b8b8"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9]

myLayoutHook =
    onWorkspace (myWorkspaces !! 0 ) ( fullSp ||| tiledSp ||| full ||| tiled) $
    onWorkspace (myWorkspaces !! 4 ) ( fullSp ||| full ) $
    onWorkspace (myWorkspaces !! 5 ) ( fullSp ||| full ) $
    onWorkspace (myWorkspaces !! 6 ) ( fullSp ||| tiledSp ||| full ||| tiled) $
    fullSp ||| tiledSp ||| full ||| tiled
    where
    tiled           = spacing 0 $ Tall master delta ratio
    tiledSp         = spacing 10 $ Tall master delta ratio
    defaultTall     = ResizableTall 1 (1/100) (1/2) []
    master          = 1
    ratio           = 1/2
    delta           = 1/100
    full            = spacing 0 $ Full
    fullSp          = spacing 10 $ Full


myManageHook = manageDocks <+> compHook <+> manageHook defaultConfig
    where compHook = composeAll
                 [ className =? "Firefox"        --> doShift "5:web"
                 , resource  =? "feh"            --> doShift "6:full"
                 , className =? "Pidgin"         --> doShift "4:mail"
                 , className =? "Thunderbird"    --> doShift "4:mail"
                 , className =? "VirtualBox"     --> doShift "7:vbox"
                 , className =? "Plasma"         --> doFloat
                 , className =? "Plasma-desktop" --> doFloat
                 ]

main = do
    spawn "setxkbmap -layout 'us,ru' -option gpr:caps_toggle"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        layoutHook         = avoidStruts $ myLayoutHook
      , workspaces         = myWorkspaces
      , modMask            = myModMask
      , terminal           = myTerminal
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myBorderColor
      , focusedBorderColor = myFocusedColor
      , manageHook         = manageHook kdeConfig <+> myManageHook
      , focusFollowsMouse  = True
      , clickJustFocuses   = True
    } `additionalKeys`
      [ (( myModMask, xK_Print ), spawn "scrot")
      , (( myModMask .|. shiftMask , xK_Print ), spawn "scrot -d 3")
      , (( controlMask, xK_F7 ), spawn "sleep 1 && xset dpms force off")
      -- apps
      , (( myModMask, xK_o ), spawn "okteta")
      , (( myModMask .|. shiftMask , xK_x ), spawn "gvim")
      , (( myModMask .|. shiftMask , xK_f ), spawn "pidgin")
      , (( myModMask .|. shiftMask , xK_p ), spawn "amarok")
      , (( myModMask .|. shiftMask , xK_t ), spawn "dolphin")
      , (( myModMask .|. shiftMask , xK_b ), spawn "firefox")
      , (( myModMask .|. shiftMask , xK_w ), spawn "wireshark")
      , (( myModMask .|. shiftMask , xK_o ), spawn "VirtualBox")
      , (( myModMask .|. shiftMask , xK_d ), spawn "monodevelop")
      , (( myModMask .|. shiftMask , xK_m ), spawn "thunderbird")
      , (( myModMask .|. shiftMask , xK_i ), spawn "/home/mathcrosp/.ida651/idaq")
      , (( myModMask .|. shiftMask , xK_n ), spawn "konsole -e bash -c alsamixer")
      , (( myModMask .|. shiftMask , xK_v ), spawn "konsole -e bash -c vifm")
      ]
      `additionalKeysP`
      [ ("M-z", spawn "konsole")
      ]

-- ending

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
myTerminal     = "urxvt"
myBorderWidth  = 5
myBorderColor  = "#363636"
myFocusedColor = "#b8b8b8"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9]

myLayoutHook =
    onWorkspace (myWorkspaces !! 0 ) ( full ||| tiled ||| fullSp ) $
    onWorkspace (myWorkspaces !! 4 ) ( full ||| fullSp ) $
    onWorkspace (myWorkspaces !! 5 ) ( full ||| fullSp ) $
    onWorkspace (myWorkspaces !! 6 ) ( full ||| tiled ||| fullSp ) $
    full ||| tiled ||| fullSp
  where
    tiled           = spacing 0 $ Tall master delta ratio
    defaultTall     = ResizableTall 1 (1/100) (1/2) []
    master          = 1
    ratio           = 1/2
    delta           = 1/100
    full            = spacing 0 $ Full
    fullSp          = spacing 40 $ Full


myManageHook = manageDocks <+> compHook <+> manageHook defaultConfig
    where compHook = composeAll
                 [ className =? "Firefox"        --> doShift "5"
                 , resource  =? "feh"            --> doShift "6"
                 , className =? "Pidgin"         --> doShift "4"
                 , className =? "Thunderbird"    --> doShift "4"
                 , className =? "VirtualBox"     --> doShift "7"
                 , className =? "Plasma"         --> doFloat
                 , className =? "Plasma-desktop" --> doFloat
                 ]

myLogHook xmproc = dynamicLogWithPP $ compPP { ppOutput = hPutStrLn xmproc }
  where
    compPP = defaultPP {
        ppHidden  = xmobarColor "#a9acb6" ""
      , ppCurrent = xmobarColor "#e2e2e2" "" . wrap "⠲" ""
      , ppUrgent  = xmobarColor "#a9acb6" "" . wrap "*" "*"
      , ppLayout  = xmobarColor "#ff0000" ""
      , ppTitle   = (\str -> "")
      , ppOrder   = \(ws:_:t:_) -> [ws, t]
      , ppSep     = "<fc=#a9acb6> | </fc>"
   }


main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/mathcrosp/.xmonad/xmobarrc.hs"
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
      , logHook            = myLogHook xmproc
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
      , (( myModMask .|. shiftMask , xK_t ), spawn "nautilus")
      , (( myModMask .|. shiftMask , xK_b ), spawn "firefox")
      , (( myModMask .|. shiftMask , xK_w ), spawn "wireshark")
      , (( myModMask .|. shiftMask , xK_p ), spawn "clementine")
      , (( myModMask .|. shiftMask , xK_o ), spawn "VirtualBox")
      , (( myModMask .|. shiftMask , xK_d ), spawn "monodevelop")
      , (( myModMask .|. shiftMask , xK_m ), spawn "thunderbird")
      , (( myModMask .|. shiftMask , xK_i ), spawn "/home/mathcrosp/.ida651/idaq")
      , (( myModMask .|. shiftMask , xK_v ), spawn "urxvt -e bash -c vifm")
      , (( myModMask .|. shiftMask , xK_n ), spawn "urxvt -e bash -c alsamixer")
      , (( myModMask .|. shiftMask , xK_z ), spawn "urxvt -e bash -c 'tmux a -t 0'")
      -- cmus
      , (( myModMask, xK_Left), spawn "cmus-remote --prev")
      , (( myModMask, xK_Down), spawn "cmus-remote --pause")
      , (( myModMask, xK_Right), spawn "cmus-remote --next")
      ]
      `additionalKeysP`
      [ ("M-z", spawn "urxvt")
      ]

-- ending

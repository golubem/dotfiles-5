-- beginning

import XMonad
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

myModMask  = mod4Mask
myTerminal = "urxvt"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..18]

myLayoutHook =
    onWorkspace (myWorkspaces !! 0 ) ( full ||| tiled ||| fullSp ) $
    onWorkspace (myWorkspaces !! 4 ) ( full ||| fullSp ) $
    onWorkspace (myWorkspaces !! 5 ) ( full ||| fullSp ) $
    onWorkspace (myWorkspaces !! 6 ) ( full ||| tiled ||| fullSp ) $
    full ||| tiled ||| fullSp
  where
    tiled       = spacing 0 $ Tall master delta ratio
    defaultTall = ResizableTall 1 (1/100) (1/2) []
    master      = 1
    ratio       = 1/2
    delta       = 1/100
    full        = spacing 0 $ Full
    fullSp      = spacing 40 $ Full


myManageHook = manageDocks <+> compHook <+> manageHook defaultConfig
    where compHook = composeAll
                 [ className =? "Firefox"    --> doShift "5"
                 , className =? "Pidgin"     --> doShift "4"
                 , className =? "Telegram"   --> doShift "4"
                 ]

myLogHook xmproc = dynamicLogWithPP $ compPP { ppOutput = hPutStrLn xmproc }
  where
    compPP      = defaultPP {
    ppHidden    = xmobarColor "#a9acb6" ""
    , ppCurrent = xmobarColor "#e2e2e2" "" . wrap "[" "]"
    , ppUrgent  = xmobarColor "#a9acb6" "" . wrap "*" "*"
    , ppLayout  = xmobarColor "#ff0000" ""
    , ppTitle   = (\str -> "")
    , ppOrder   = \(ws:_:t:_) -> [ws, t]
    , ppSep     = "<fc=#a9acb6> | </fc>"
    }

myKeys =
      [ (( myModMask, xK_Print ), spawn "scrot")
      , (( controlMask, xK_F7 ), spawn "sleep 1 && xset dpms force off")
      -- apps
      , (( myModMask, xK_z ), spawn "urxvt")
      , (( myModMask .|. shiftMask , xK_h ), spawn "urxvt -e bash -c htop")
      , (( myModMask .|. shiftMask , xK_a ), spawn "urxvt -e bash -c atop")
      , (( myModMask .|. shiftMask , xK_v ), spawn "urxvt -e bash -c vifm")
      , (( myModMask .|. shiftMask , xK_z ), spawn "urxvt -e bash -c 'tmux a -t 0'")
      , (( myModMask, xK_d ), spawn "dmenu_run -nb '#080808' -sb '#d1d1d1' -sf '#080808'")
      -- workspaces
      , (( myModMask, xK_0), windows $ W.greedyView "10")
      , (( shiftMask .|. myModMask, xK_0), windows $ W.shift "10")
      , (( myModMask, xK_F1), windows $ W.greedyView "11")
      , (( shiftMask .|. myModMask, xK_F1), windows $ W.shift "11")
      , (( myModMask, xK_F2), windows $ W.greedyView "12")
      , (( shiftMask .|. myModMask, xK_F2), windows $ W.shift "12")
      , (( myModMask, xK_F3), windows $ W.greedyView "13")
      , (( shiftMask .|. myModMask, xK_F3), windows $ W.shift "13")
      , (( myModMask, xK_F4), windows $ W.greedyView "14")
      , (( shiftMask .|. myModMask, xK_F4), windows $ W.shift "14")
      , (( myModMask, xK_F5), windows $ W.greedyView "15")
      , (( shiftMask .|. myModMask, xK_F5), windows $ W.shift "15")
      , (( myModMask, xK_F6), windows $ W.greedyView "16")
      , (( shiftMask .|. myModMask, xK_F6), windows $ W.shift "16")
      , (( myModMask, xK_F7), windows $ W.greedyView "17")
      , (( shiftMask .|. myModMask, xK_F7), windows $ W.shift "17")
      , (( myModMask, xK_F8), windows $ W.greedyView "18")
      , (( shiftMask .|. myModMask, xK_F8), windows $ W.shift "18")
      ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/mathcrosp/.xmonad/xmobarrc.hs"
    spawn "setxkbmap -layout 'us,ru' -option gpr:caps_toggle"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        layoutHook         = avoidStruts $ myLayoutHook
      , workspaces         = myWorkspaces
      , modMask            = myModMask
      , terminal           = myTerminal
      , manageHook         = myManageHook
      , logHook            = myLogHook xmproc
      , focusFollowsMouse  = True
      , clickJustFocuses   = True
    } `additionalKeys` myKeys

-- ending

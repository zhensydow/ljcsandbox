import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Tabbed
import System.IO

main = do
  xmproc <- spawnPipe "/usr/local/bin/xmobar /home/cabellos/.xmonad/xmobar"
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ myLayout
       , logHook = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50 }
       , modMask = mod4Mask 
       , terminal           = "urxvt"
       , focusFollowsMouse  = False }

myLayout = tiled ||| Mirror tiled||| simpleTabbedAlways
    where
      tiled = Tall 1 0.03 (1/2)
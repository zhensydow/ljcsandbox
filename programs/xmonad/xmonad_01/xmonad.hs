import XMonad( terminal, workspaces, manageHook, layoutHook, logHook, (<+>), modMask, controlMask, mod4Mask, focusFollowsMouse 
             , Full(..), Mirror(..), Tall(..), (|||) )
import XMonad.Main( xmonad )
import XMonad.Config( defaultConfig )
import XMonad.ManageHook( resource, doIgnore, (=?) )
import XMonad.Util.Run( spawnPipe )
import XMonad.Hooks.DynamicLog( dynamicLogWithPP, PP(..), xmobarColor, xmobarPP, wrap, shorten )
import XMonad.Hooks.ManageDocks( manageDocks, avoidStruts, ToggleStruts(..)  )
import XMonad.Hooks.ManageHelpers( composeOne, (-?>) )
import XMonad.Layout.Gaps( GapMessage(..), gaps, Direction2D(..) )
import XMonad.Layout.ShowWName( showWName', defaultSWNConfig, SWNConfig(..) )
import XMonad.Layout.PerWorkspace( onWorkspace )
import XMonad.Layout.NoBorders( smartBorders )
import XMonad.Layout.Tabbed( tabbed, defaultTheme, shrinkText, Theme(..) )
import System.IO( hPutStrLn )

main = do
  xmproc <- spawnPipe "exec /usr/local/bin/xmobar /home/cabellos/.xmonad/xmobar"
  xmonad $ defaultConfig
       { terminal           = "exec urxvt"
       , manageHook = (composeOne [ resource =? "stalonetray" -?> doIgnore ]) <+> manageDocks <+> manageHook defaultConfig
       , layoutHook = showWName' mySWNConfig myLayout
       , workspaces = myWorkspaces

       -- xmobar configuration
       , logHook = dynamicLogWithPP $ xmobarPP 
                   { ppOutput = hPutStrLn xmproc . wrap "< " " >"
                   , ppCurrent = xmobarColor "yellow" "" . (wrap "[" "]" ) . (drop 2)
                   , ppVisible = (\_->"")
                   , ppHidden = xmobarColor "red" "" . (take 1)
                   , ppHiddenNoWindows = take 1
                   , ppLayout = (\_->"")
                   , ppUrgent = (\_->"")
                   , ppSep = " >-< "
                   , ppTitle = xmobarColor "green" "" . shorten 50 }

       -- keys
       , modMask = mod4Mask 
       , focusFollowsMouse  = False }


web = "1:web"
myWorkspaces = [ web,"2:code1","3:code2","4:apps", "5:apps" ]

myLayout = onWorkspace web (gaps [(U,16)] $ avoidStruts $ smartBorders myTabbed) $
           gaps [(U,16)] $ avoidStruts $ smartBorders $ (normalTiled ||| Mirror normalTiled ||| myTabbed)
    where
      normalTiled = Tall 1 (2/100) (1/2)
      myTabbed = tabbed shrinkText myTabConfig

mySWNConfig = defaultSWNConfig
              { swn_font    = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
              , swn_color   = "green"
              , swn_fade    = 1/4 }

myTabConfig = defaultTheme 
              { activeColor = "#007046"
              , activeTextColor = "#FFFFFF"
              , activeBorderColor = "#20815D"
              , inactiveColor = "#0C0874"
              , inactiveTextColor = "#AAAAAA"
              , inactiveBorderColor = "#333086"
              , decoHeight = 12 }

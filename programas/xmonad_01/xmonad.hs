import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.PerWorkspace
import System.IO
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe "/usr/local/bin/xmobar /home/cabellos/.xmonad/xmobar"
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> manageHook defaultConfig
       , layoutHook = showWName' mySWNConfig $ avoidStruts $ myLayout
       , logHook = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50 }
       -- keys
       , modMask = mod4Mask 
       , keys = \c -> myKeys c `M.union` keys defaultConfig c
       , terminal           = "urxvt"
       , workspaces = myWorkspaces
       , focusFollowsMouse  = False }

webName = "1:web"
altName = "2:altamira"
myWorkspaces = [
 webName,altName,"3:apps01",
 "4:apps02","5:apps03", "6:pidgin"]

mySWNConfig = defaultSWNConfig
              { swn_font    = "-misc-fixed-*-*-*-*-14-*-*-*-*-*-*-*"
              , swn_color   = "green"
              , swn_fade    = 1/4 }

myLayout = avoidStruts $ smartBorders $
           onWorkspace webName simpleTabbedAlways $ 
           onWorkspace altName (Mirror tiled ||| simpleTabbed) $
           tiled ||| Mirror tiled ||| simpleTabbed
    where
      tiled = Tall 1 0.03 (1/2)

myKeys conf@(XConfig {modMask = modm}) = 
    M.fromList $
         -- Lanzar aplicaciones comunes
         [ ((modm, xK_e), spawn "emacs" ) 
         -- Full Screen
         , ((modm, xK_b), sendMessage ToggleStruts )
         -- Configuracion para navegar entre ventanas
         , ((modm, xK_Left), prevWS ) 
         , ((modm, xK_Right), nextWS ) 
         , ((modm, xK_Down  ), windows W.focusDown)
         , ((modm, xK_Up    ), windows W.focusUp) 
         , ((modm, xK_Tab), toggleWS) ]

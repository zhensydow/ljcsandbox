import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Prompt
import XMonad.Actions.Submap
import XMonad.Actions.Search as Search
import qualified Data.Map as M

main = xmonad $ defaultConfig
       { terminal = "xterm"
       , workspaces = myWorkspaces
       -- xmonad style
       , borderWidth = 1
       , normalBorderColor = "#aa99aa"
       , focusedBorderColor = "#10aa10"
       , focusFollowsMouse = False
       -- use windows key 
       , modMask = mod4Mask 
       , keys = \c -> myKeys c `M.union` keys defaultConfig c
       -- gnome-esque configuration
       , manageHook = manageDocks <+> manageHook defaultConfig
       , logHook = ewmhDesktopsLogHook 
       , layoutHook = showWName' mySWNConfig myLayout }

mySWNConfig = defaultSWNConfig
              { swn_font    = "-misc-fixed-*-*-*-*-14-*-*-*-*-*-*-*"
              , swn_color   = "green"
              , swn_fade    = 1/4 }

web = "1:web"
alta01 = "2:altamira01"
pidgin = "7:pidgin"

myWorkspaces = [
 web,alta01,"3:altamira02","4:apps",
 "5:code01","6:code02", pidgin]
                      
myKeys conf@(XConfig {modMask = modm}) = 
    M.fromList $
         -- Lanzar aplicaciones comunes
         [ ((modm, xK_e), spawn "emacs" ) 
         , ((modm, xK_f), spawn "firefox" )
         , ((modm .|. controlMask, xK_2), do
              windows $ W.greedyView alta01
              spawn $ XMonad.terminal conf 
              spawn $ XMonad.terminal conf 
              spawn $ XMonad.terminal conf )
         , ((modm .|. shiftMask, xK_e), spawn "emacs ~/.xmonad/xmonad.hs" )
         -- Full Screen
         , ((modm, xK_b), sendMessage ToggleStruts )
         -- Configuracion para navegar entre ventanas
         , ((modm, xK_Left), prevWS ) 
         , ((modm, xK_Right), nextWS )
         , ((modm .|. shiftMask , xK_Left), shiftToPrev )
         , ((modm .|. shiftMask , xK_Down), shiftToNext ) 
         , ((modm, xK_Down  ), windows W.focusDown)
         , ((modm, xK_Up    ), windows W.focusUp) 
         , ((modm, xK_Tab), toggleWS) 
         -- Busquedas
         , ((modm, xK_s), submap $ searchMap $ Search.promptSearch defaultXPConfig) 
         , ((modm .|. shiftMask, xK_s), submap $ searchMap $ Search.selectSearch) ]

searchMap method = M.fromList $
                   [ ((0, xK_g), do
                        windows $ W.greedyView web
                        method "firefox" Search.google ) 
                   , ((0, xK_w), do
                        windows $ W.greedyView web
                        method "firefox" Search.wikipedia )
                   , ((0, xK_i), do
                        windows $ W.greedyView web
                        method "firefox" Search.imdb ) 
                   , ((0, xK_m), do
                        windows $ W.greedyView web
                        method "firefox" Search.maps ) ]

myLayout = onWorkspace web (avoidStruts $ smartBorders simpleTabbed) $
           onWorkspace alta01 (Mirror extraFitTiled ||| simpleTabbed) $
           onWorkspace pidgin (avoidStruts $ (fitTiled ||| simpleTabbed)) $
           avoidStruts $ smartBorders $ (normalTiled ||| Mirror normalTiled ||| simpleTabbed)
    where
      normalTiled = Tall 1 (3/100) (1/2)
      fitTiled = Tall 1 (3/100) (1/4)
      extraFitTiled = Tall 1 (3/100) (1/5)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Tabbed
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.LayoutCombinators((*/*))
import XMonad.Layout.Gaps
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Themes
import XMonad.Prompt.Ssh
import XMonad.Actions.Submap
import XMonad.Actions.Search as Search
import System.IO
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe "/usr/local/bin/xmobar /home/cabellos/.xmonad/xmobar"
  xmonad $ defaultConfig
             { terminal = "urxvt"
             , manageHook = manageDocks <+> manageHook defaultConfig
             , layoutHook = showWName' mySWNConfig myLayout

             -- xmobar configuration
             , logHook = dynamicLogWithPP $ xmobarPP 
                         { ppOutput = hPutStrLn xmproc . wrap "< " " >"
                         , ppCurrent = 
                             xmobarColor "yellow" "" . (wrap "[" "]" ) . (drop 2)
                         , ppVisible = (\_->"")
                         , ppHidden = xmobarColor "red" "" . (take 1)
                         , ppHiddenNoWindows = take 1
                         , ppLayout = (\_->"")
                         , ppUrgent = (\_->"")
                         , ppSep = " >-< "
                         , ppTitle = xmobarColor "green" "" . shorten 50 }
             , workspaces = myWorkspaces

             -- Key configuration 
             , modMask = mod4Mask
             , keys = \c -> myKeys c `M.union` keys defaultConfig c

             -- xmonad style
             , borderWidth = 1
             , normalBorderColor = "#aa99aa"
             , focusedBorderColor = "#10aa10"
             , focusFollowsMouse = False
             }

mySWNConfig = defaultSWNConfig
              { swn_font    = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
              , swn_color   = "green"
              , swn_fade    = 1/4 }

myKeys conf@(XConfig {modMask = modm}) = 
    M.fromList $
         -- Lanzar aplicaciones comunes
         [ ((modm, xK_e), spawn "emacs" ) 
         , ((modm, xK_i), submap internetMap )
         , ((modm .|. shiftMask, xK_e), spawn "emacs ~/.xmonad/xmonad.hs" )
         , ((modm, xK_n), spawn "thunar" )
         , ((modm .|. controlMask, xK_l), spawn "xscreensaver-command -lock" )
         , ((modm, xK_m), spawn "urxvt -e sm" )
         , ((modm, xK_d), spawn "(echo Calendario; cal; sleep 8) | dzen2 -x 30 -y 30 -w 160 -bg '#aaffaa' -fg black -l 8 -e onstart=togglecollapse &" )
         , ((modm, xK_a), spawn "urxvt -e aumix" )
         -- Full Screen
         , ((modm, xK_b), do
              sendMessage ToggleStruts 
              sendMessage ToggleGaps )
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

internetMap = M.fromList $
              [ ((0, xK_f), spawn "chromium-browser --enable-plugins" )
              , ((0, xK_p), spawn "pidgin" )
              , ((0, xK_s), sshPrompt defaultXPConfig ) ]

mySearch s = do
  s
  windows $ W.greedyView web

searchMap method = M.fromList $
                   [ ((0, xK_g), mySearch $ method Search.google )
                   , ((0, xK_w), mySearch $ method Search.wikipedia )
                   , ((0, xK_i), mySearch $ method Search.imdb ) 
                   , ((0, xK_m), mySearch $ method Search.maps ) 
                   , ((0, xK_d), mySearch $ method Search.mathworld )
                   , ((0, xK_y), mySearch $ method Search.youtube ) ]

web = "1:web"
media = "2:multimedia"
pidgin = "6:pidgin"

myWorkspaces = [ web,media,"3:code01","4:code02","5:apps",pidgin ]

--myLayout = onWorkspace web (gaps [(L,100)] $ avoidStruts $ smartBorders myTabbed) $
myLayout = onWorkspace web (avoidStruts $ smartBorders myTabbed) $
           onWorkspace media (noBorders Full) $
           onWorkspace pidgin (avoidStruts $ (fitTiled ||| myTabbed)) $
           avoidStruts $ smartBorders $ (normalTiled ||| Mirror normalTiled ||| myTabbed)
    where
      normalTiled = Tall 1 (2/100) (1/2)
      fitTiled = Tall 1 (2/100) (1/4)
      myTabbed = tabbed shrinkText myTabConfig

myTabConfig = defaultTheme 
              { activeColor = "#007046"
              , activeTextColor = "#FFFFFF"
              , activeBorderColor = "#20815D"
              , inactiveColor = "#0C0874"
              , inactiveTextColor = "#AAAAAA"
              , inactiveBorderColor = "#333086"
              , decoHeight = 12 }

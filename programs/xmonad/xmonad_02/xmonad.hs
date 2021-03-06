import XMonad( terminal, workspaces, manageHook, (<+>), logHook, layoutHook, spawn, shiftMask, windows
             , keys, modMask, controlMask, mod4Mask, XConfig(..), (.|.), sendMessage
             , xK_a, xK_b, xK_d, xK_e, xK_f, xK_g, xK_i, xK_l, xK_m, xK_n, xK_p, xK_s, xK_w, xK_y
             , xK_Up, xK_Down, xK_Left, xK_Right, xK_Tab
             , borderWidth, normalBorderColor, focusedBorderColor, focusFollowsMouse
             , Full(..), Mirror(..), Tall(..), (|||) )
import qualified XMonad.StackSet as W( focusDown, focusUp, greedyView )
import XMonad.Main( xmonad )
import XMonad.Config( defaultConfig )
import XMonad.ManageHook( resource, doIgnore, (=?) )
import XMonad.Util.Run( spawnPipe )
import XMonad.Hooks.ManageDocks( manageDocks, avoidStruts, ToggleStruts(..) )
import XMonad.Hooks.ManageHelpers( composeOne, (-?>) )
import XMonad.Hooks.DynamicLog( dynamicLogWithPP, PP(..), xmobarColor, xmobarPP, wrap, shorten )
import XMonad.Hooks.SetWMName( setWMName )
import XMonad.Actions.CycleWS( prevWS, nextWS, shiftToPrev, shiftToNext, toggleWS )
import XMonad.Actions.Submap( submap )
import XMonad.Layout.Gaps( GapMessage(..), gaps, Direction2D(..) )
import XMonad.Layout.ShowWName( showWName', defaultSWNConfig, SWNConfig(..) )
import XMonad.Layout.PerWorkspace( onWorkspace )
import XMonad.Layout.NoBorders( noBorders, smartBorders )
import XMonad.Layout.Tabbed( tabbed, defaultTheme, shrinkText, Theme(..) )
import XMonad.Prompt( defaultXPConfig )
import XMonad.Prompt.Ssh( sshPrompt )
import XMonad.Actions.Search as Search( promptSearch, selectSearch
                                      , imdb, google, wikipedia, maps, mathworld, youtube )
import XMonad.Layout.LayoutCombinators((*/*))
import System.IO( hPutStrLn )
import qualified Data.Map as M( union, fromList )

main = do
  xmproc <- spawnPipe "exec /usr/local/bin/xmobar /home/cabellos/.xmonad/xmobar"
  xmonad $ defaultConfig
       { terminal = "exec urxvt" 
       , manageHook = (composeOne [ resource =? "stalonetray" -?> doIgnore ]) <+> manageDocks <+> manageHook defaultConfig
       , layoutHook = showWName' mySWNConfig myLayout
       , workspaces = myWorkspaces

       , startupHook = setWMName "LG3D"

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

       -- Key configuration 
       , modMask = mod4Mask
       , keys = \c -> myKeys c `M.union` keys defaultConfig c

       -- xmonad style
       , borderWidth = 1
       , normalBorderColor = "#aa99aa"
       , focusedBorderColor = "#10aa10"
       , focusFollowsMouse = False
       }

web = "1:web"
media = "2:media"
pidgin = "6:pidgin"

myWorkspaces = [ web,media,"3:code1","4:code2","5:apps",pidgin ]

myLayout = onWorkspace web (gaps [(U,16)] $ avoidStruts $ smartBorders myTabbed) $
           onWorkspace media (noBorders Full) $
           onWorkspace pidgin (gaps [(U,16)] $ avoidStruts $ (fitTiled ||| myTabbed)) $
           gaps [(U,16)] $ avoidStruts $ smartBorders $ (normalTiled ||| Mirror normalTiled ||| myTabbed)
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

mySWNConfig = defaultSWNConfig
              { swn_font    = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
              , swn_color   = "green"
              , swn_fade    = 1/4 }

myKeys conf@(XConfig {modMask = modm}) = 
    M.fromList $
         -- Lanzar aplicaciones comunes
         [ ((modm, xK_e), spawn "exec emacs" ) 
         , ((modm, xK_i), submap internetMap )
         , ((modm .|. shiftMask, xK_e), spawn "exec emacs ~/.xmonad/xmonad.hs" )
         , ((modm, xK_n), spawn "exec thunar" )
         , ((modm .|. controlMask, xK_l), spawn "exec xscreensaver-command -lock" )
         , ((modm, xK_m), spawn "exec urxvt -e sm" )
         , ((modm, xK_d), spawn "exec (echo Calendario; cal; sleep 8) | dzen2 -x 30 -y 30 -w 160 -bg '#aaffaa' -fg black -l 8 -e onstart=togglecollapse &" )
         , ((modm, xK_a), spawn "exec urxvt -e aumix" )
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
         , ((modm .|. shiftMask, xK_s), submap $ searchMap $ Search.selectSearch) 
         ]

internetMap = M.fromList $
              [ ((0, xK_f), spawn "exec chromium-browser --enable-plugins --enable-file-cookies" )
              , ((0, xK_p), spawn "exec ssh -X ozzy.ifca.es pidgin" )
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

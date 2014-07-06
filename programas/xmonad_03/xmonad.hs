-- http://bitelia.com/2010/06/gnome-xmonad-en-ubuntu-1004-i-instalacion
-- http://www.nepherte.be/howto-step-by-step-configuration-of-xmonad/
-- aplicaciones adicionales a instalar   xrvt-unicode  dzen2  dmenu aumix
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.Search as Search
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Gaps
import XMonad.Layout.ShowWName
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.Ssh
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
  xmonad $ gnomeConfig {
    terminal = "exec urxvt"
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = showWName' mySWNConfig myLayout
    , workspaces = myWorkspaces
                   
    , modMask = mod4Mask
    , keys = \c -> myKeys c `M.union` keys defaultConfig c
                      
    , normalBorderColor = "#aa99aa"
    , focusedBorderColor = "#10aa10"
    , borderWidth = 1
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
         , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
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
              , ((0, xK_p), spawn "exec pidgin" )
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

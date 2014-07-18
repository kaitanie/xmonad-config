{- xmonad.hs
 - Author: Jelle van der Waa ( jelly12gen ), modifications by Pekka Kaitaniemi
 -}

-- Import stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO


-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect

-- utils
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt          as P
import XMonad.Prompt.Shell
import XMonad.Prompt


-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

-- Data.Ratio for IM layout
import Data.Ratio ((%))

import XMonad.Layout.Circle

import XMonad.Layout.DwmStyle

import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops

-- Main --
main = do
--        xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"  -- start xmobar
--        gnomeSettindsDaemonProc <- spawn "gnome-settings-daemon"
--        bluetoothAppletProc <- spawn "bluetooth-applet"
--        nmAppletProc <- spawn "nm-applet --sm-disable"
--        _ <- spawn "/usr/bin/xcompmgr -n"
--        _ <- spawn "xset -b" -- Disable the system bell
--        _ <- spawn "xscreensaver -nosplash"
--        _ <- spawn "thunar --daemon"
        _ <- spawn "xrdb load /dev/null" -- Clear the X resource DB to work around CentOS 7 bug
        _ <- spawn "xrdb merge ~/.xmonad/Xresources" -- Read the custom Xresources
        -- trayerProc <- spawn "trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand false --width 10 --transparent true --tint 0x191970 --height 12"
        xmodmapProc <- spawn "xmodmap ~/.xmonad/xmodmap-settings" -- enable our keyboard settings (make Caps Lock an additional CTRL etc.)
        xmonad  $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
                { manageHook = myManageHook
                , layoutHook = newLayoutHook
                , borderWidth = myBorderWidth
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys = myKeys
                , logHook = setWMName "LG3D"  -- >> (myLogHook xmproc)
                , modMask = myModMask
                , terminal = myTerminal
                , workspaces = myWorkspaces
                , focusFollowsMouse = True
                , startupHook = setWMName "LG3D"
                }


-- hooks
-- automaticly switching app to workspace
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.45) <+> ( composeAll . concat $
                [[isFullscreen                  --> doFullFloat
                , className =? "OpenOffice.org 3.1" --> doShift "5:doc"
                , className =? "OpenOffice.org" --> doShift "5:doc"
                , className =?  "Xmessage"      --> doCenterFloat
                , className =?  "Zenity"        --> doCenterFloat
                , className =? "feh"            --> doCenterFloat
                , className =? "Pidgin"         --> doShift "1:chat"
                , className =? "Kopete"         --> doShift "1:chat"
                , className =? "Ekiga"          --> doShift "1:chat"
                , className =? "Gimp"           --> doShift "9:gimp"
                , className =? "uzbl"           --> doShift "2:web"
                , className =? "vimprobable"    --> doShift "2:web"
                , className =? "Pidgin"         --> doShift "1:chat"
--                , className =? "Skype"          --> doShift "1:chat"
                , className =? "MPlayer"	--> doShift "8:vid"
                , className =? "VirtualBox"	--> doShift "6:virtual"
                , className =? "Apvlv"          --> doShift "4:pdf"
                , className =? "Evince"         --> doShift "4:pdf"
                , className =? "Epdfview"       --> doShift "4:pdf"
                , className =? "Okular"         --> doShift "4:pdf"
                , className =? "Remmina"        --> doShift "6:geant4"
                , className =? "Trayer"         --> doShift "1:chat"]
                ]
                        )  <+> manageDocks



--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }



---- Looks --
---- bar
customPP :: PP
customPP = defaultPP {
                            ppHidden = xmobarColor "#00FF00" ""
                          , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
                          , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
                          , ppLayout = xmobarColor "#FF0000" ""
                          , ppTitle = xmobarColor "#00FF00" "" . shorten 80
                          , ppSep = "<fc=#0033FF> | </fc>"
                     }

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig
    {
        font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
        ,fgColor = "#00FFFF"
        , bgColor = "#000000"
        , bgHLight    = "#000000"
        , fgHLight    = "#FF0000"
        , position = Top
    }

--- My Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

newLayoutHook =  onWorkspace "1:chat" imLayout $ onWorkspace "2:web" webL $ onWorkspace "3:mail" imLayout $ onWorkspace "4:pdf" programmingL $ onWorkspace "5:doc" writingL $ onWorkspace "6:lisp" programmingL $ onWorkspace "7:grid" programmingL $ onWorkspace "8:code" programmingL $ onWorkspace "9:vid" fullL $ standardLayouts
   where
        standardLayouts =   avoidStruts  $ (tabLayout ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid ||| Full ||| Circle)

        --Layouts
--        dwmTiled = dwmStyle shrinkText defaultTheme (layoutHook defaultConfig)
--        reflectDwmTiled = (reflectHoriz dwmTiled)
        tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbed shrinkText myTheme)
        full      = noBorders Full

        --Im Layout
        imLayout = avoidStruts $ smartBorders $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| tiled ||| Grid) where
                chatLayout      = Grid
                ratio = (1%9)
                skypeRatio = (1%8)
                pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
                skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

        programmingL = avoidStruts  $ (tabLayout ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid ||| Full)

        --Gimp Layout
--	gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full

        --Web Layout
        webL      = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full ||| Mirror tiled ||| reflectTiled

        --Writing layout
        writingL = avoidStruts $ (Mirror tiled ||| tabLayout ||| tiled |||  reflectTiled ||| Grid ||| Full)

        --VirtualLayout
        fullL = avoidStruts $ full ||| tiled ||| tabLayout
--        fullL = zoomRow ||| Mirror zoomRow
--        fullL = avoidStruts $ tabLayout ||| tiled ||| reflectHoriz tiled


--LayoutHook
myLayoutHook  =  onWorkspace "1:chat" imLayout $  onWorkspace "2:web" webL $ onWorkspace "3:code" programmingL $ onWorkspace "5:doc" writingL $ onWorkspace "6:geant4" programmingL $ onWorkspace "9:gimp" gimpL $ onWorkspace "6:games" fullL $ onWorkspace "8:vid" fullL $ standardLayouts
   where
        standardLayouts =   avoidStruts  $ (tabLayout ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid ||| Full ||| Circle)

        --Layouts
--        dwmTiled = dwmStyle shrinkText defaultTheme (layoutHook defaultConfig)
--        reflectDwmTiled = (reflectHoriz dwmTiled)
        tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbed shrinkText myTheme)
        full      = noBorders Full

        --Im Layout
        imLayout = avoidStruts $ smartBorders $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| tiled ||| Grid) where
                chatLayout      = Grid
                ratio = (1%9)
                skypeRatio = (1%8)
                pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
                skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

        programmingL = avoidStruts  $ (tabLayout ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid ||| Full)

        --Gimp Layout
        gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full

        --Web Layout
        webL      = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full ||| Mirror tiled ||| reflectTiled

        --Writing layout
        writingL = avoidStruts $ (Mirror tiled ||| tabLayout ||| tiled |||  reflectTiled ||| Grid ||| Full)

        --VirtualLayout
        fullL = avoidStruts $ full ||| tiled ||| tabLayout
--        fullL = zoomRow ||| Mirror zoomRow
--        fullL = avoidStruts $ tabLayout ||| tiled ||| reflectHoriz tiled




-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "xterm"

myEditor :: String
myEditor = "emacs"

myScratchpad :: String
myScratchpad = "emacs ~/Scratchpad.org"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
--myModMask = mod4Mask
myModMask = mod5Mask -- AltGr

-- borders
myBorderWidth :: Dimension
myBorderWidth = 1
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#FF0000"
--

--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat", "2:web", "3:mail", "4:pdf", "5:doc", "6:code" ,"7:lisp", "8:grid", "9:vid"]
--

-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "2:web")                           -- (0,0a)
--

--Search engines to be selected :  [google (g), wikipedia (w) , youtube (y) , maps (m), dictionary (d) , wikipedia (w), bbs (b) ,aur (r), wiki (a) , TPB (t), mininova (n), isohunt (i) ]
--keybinding: hit mod + s + <searchengine>
searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google )
       , ((0, xK_m), method S.maps )
       , ((0, xK_w), method S.wikipedia )
       , ((0, xK_h), method S.hoogle )
       , ((0, xK_i), method S.isohunt )
       , ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
       ]


ssh = "ssh -i /path/to/file/.key usere@domain "
-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c ), kill)

    -- opening program launcher / search engine
--    , ((modMask , xK_s ), SM.submap $ searchEngineMap $ S.promptSearchBrowser myXPConfig "chromium")
    ,((modMask , xK_p), shellPrompt myXPConfig)


    -- GridSelect
    , ((modMask, xK_g), goToSelected defaultGSConfig)

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask, xK_n ), refresh)

    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)


    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )

    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)

    -- mpd controls
    , ((0                       , 0x1008ff16 ), spawn (ssh ++ "ncmpcpp prev"))
    , ((0                       , 0x1008ff17 ), spawn (ssh ++ "ncmpcpp next"))
    , ((0                       , 0x1008ff14 ), spawn (ssh ++ "ncmpcpp play"))
    , ((0                       , 0x1008ff15 ), spawn (ssh ++"ncmpcpp pause"))

    -- scratchpad
--    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal})
     , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myScratchpad})

    --Programs
    , ((modMask .|.  shiftMask, xK_p ), spawn "kopete")
    , ((modMask .|.  shiftMask, xK_b ), spawn "google-chrome")
    , ((modMask .|.  shiftMask, xK_x ), spawn "xterm")
    , ((modMask .|.  shiftMask, xK_s ), spawn "unison -terse -batch keep")
    , ((modMask, xK_z ),                spawn "emacs")

    --Keyboard layout
    , ((modMask .|.  shiftMask, xK_u ), spawn "~/.xmonad/bin/keymap.sh gb")
    , ((modMask .|.  shiftMask, xK_f ), spawn "~/.xmonad/bin/keymap.sh fi")

    -- volume control
    , ((0                       , 0x1008ff13 ), spawn "amixer -q set Master 2dB+")
    , ((0                       , 0x1008ff11 ), spawn "amixer -q set Master 2dB-")
    , ((0                       , 0x1008ff12 ), spawn "amixer -q set Master toggle")

    -- Lock screen
    , ((modMask .|. shiftMask, xK_y), spawn "xscreensaver-command -lock")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

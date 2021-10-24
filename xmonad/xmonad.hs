import Control.Monad
import Data.Ratio
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["1:shell","2:browser","3:browser","4","5","6","7","8","9"]
myWorkspaces    = ["1:shell","2:br1","3:br2", "4:mail/db"]
  ++ map show [ 5 .. 7 ]
  ++ ["8:signal", "9:slack"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ffffff"

------------------------------------------------------------------------
    -- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
      , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
      , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
    -- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = do
    avoidStruts $ smartBorders $ smartSpacing 3 $ renamed [Replace "|=" ] tiled |||  renamed [Replace "[]"] Full ||| renamed [Replace "=="] Grid
        where
            tiled = ResizableTall 1 (2/100) (1/2) []


------------------------------------------------------------------------
    -- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll [
      className =? "Alacritty"        --> doShift "1:shell"
        , className =? "URxvt"            --> doShift "1:shell"
        , className =? "st-256color"      --> doShift "1:shell"
        , className =? "Emacs"            --> doShift "1:shell"
        , className =? "firefox"          --> doShift "2:br1"
        , className =? "Firefox"          --> doShift "2:br1"
        , className =? "Chromium"         --> doShift "3:br2"
        , className =? "Thunderbird"      --> doShift "4:mail/db"
        , className =? "Signal"           --> doShift "8:signal"
        , className =? "Slack"            --> doShift "9:slack"
        , title =? "Dbeaver"              --> doShift "4:mail/db"
        , className =? "DBeaver"          --> doShift "4:mail/db"
        , title =? "Quit and close tabs?" --> doIgnore
        , title =? "Close tabs?"          --> doIgnore
        , title =? "Open File"            --> doIgnore
        , title =? "Save As"              --> doIgnore
        , resource  =? "desktop_window"   --> doIgnore
                          ]

------------------------------------------------------------------------
    -- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = mempty

-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
commands = [
            "randomwall"
           , "gpgconf --reload gpg-agent"
           , "xset -dpms"
           , "xset s off"
           , "picom -bc"
           , "xscreensaver -no-splash"
           , "emacs --bg-daemon"
           , "xset r rate 250 25"
           , "xset b off"
           , "dropbox start"
           , "redshift"
           , "tmuxcopy"
           , "dunst"
           , "setxkbmap -option compose:menu"
           , "setxkbmap -option caps:none"
           , "xsetroot -cursor_name left_ptr"
           ]
myStartupHook = do
    forM commands (\c -> spawnOnce c )
    -- nScreens <- countScreens
    -- forM [1..nScreens ] (\sc -> spawnOnce ("statusloop " ++ show sc))
    spawnOnce "getallmail"

main = do
    -- let dzncmd = "dzen2 -dock -ta l -tw 1200 -fn mono:size=10 -xs "
    -- nScreens <- countScreens
    -- handles <- forM [1..nScreens] (\sc -> spawnPipe (dzncmd ++ show sc))
    xmonad  $ ewmh $ withUrgencyHook NoUrgencyHook $ def
        { terminal           = myTerminal
          , focusFollowsMouse  = myFocusFollowsMouse
          , borderWidth        = myBorderWidth
          , manageHook         =  myManageHook
          , layoutHook         =  myLayout
          , modMask            = myModMask
          , normalBorderColor  = myNormalBorderColor
          , focusedBorderColor = myFocusedBorderColor
          , workspaces         = myWorkspaces
          -- , logHook            = dynamicLogWithPP def
          --     { ppOutput       = \str -> forM_ handles (flip hPutStrLn str)
          --       , ppExtras       = [ windowCount ]
          --       , ppCurrent      = dzenColor "white" ""
          --       , ppVisible      = dzenColor "grey" ""
          --       , ppHidden       = dzenColor "grey" ""
          --       , ppTitle        = dzenColor "white" "" . shorten 550
          --       , ppLayout       = dzenColor "white" ""
          --       , ppUrgent       = dzenColor "red" "" . shorten 50 . dzenStrip
          --       , ppSep          = " - "
          --       , ppOrder = \(a:b:c:d) -> [ "-" ] ++ [ b ] ++ d  ++ [ a ] ++ [ c ]
          --     }
                , startupHook        = myStartupHook
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
          , ((0, xK_Print), spawn "~/.config/dwm/scripts/screenshot")
          , ((0, xK_Pause), spawn "dnd")
          , ((shiftMask, xK_F12), spawn "systemctl poweroff")
          , ((mod4Mask, xK_m), spawn "firefox --new-tab about:blank")
          , ((mod4Mask .|. controlMask, xK_m), spawn "firefox --new-window about:blank")
          , ((mod4Mask .|. shiftMask, xK_m), spawn "firefox --private-window")
          , ((mod4Mask .|. shiftMask, xK_g), spawn "chromium --incognito")
          -- , ((mod4Mask              , xK_Return     ), spawn myTerminal)
          , ((mod4Mask              , xK_p     ), spawn "passmenu")
          , ((controlMask,               xK_space     ), spawn "rofi_run")
          , ((mod4Mask .|. controlMask , xK_r  ), spawn "restatus")
          , (( 0, xF86XK_AudioLowerVolume  ), spawn  "amixer -c 0 -- set Master 1-")
          , (( 0, xF86XK_AudioRaiseVolume  ), spawn "amixer -c 0 -- set Master 1+")
          , (( 0, xF86XK_MonBrightnessUp   ), spawn "xbacklight -inc 2")
          , (( 0, xF86XK_MonBrightnessDown ), spawn "xbacklight -dec 2")
          , (( mod4Mask, xK_Tab ), toggleWS)
          , (( mod4Mask,  xK_z  ), warpToWindow (1%2) (1%2)) -- @@ Move pointer to currently focused window
          , (( mod4Mask,  xK_F4  ), spawn "xrandr --output HDMI1 --off;  xrandr --output HDMI1 --auto")
          , (( mod4Mask .|. shiftMask, xK_s   ), setSpacing 0)
          , (( mod4Mask .|. shiftMask, xK_f   ), spawn "pcmanfm")
        ]

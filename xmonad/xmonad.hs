import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import System.Exit (exitWith, ExitCode(..))
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, removeKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "st"

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
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

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
myLayout = smartBorders $ tiled |||  Full ||| Mirror tiled ||| simpleFloat
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
      className =? "Alacritty"      --> doShift "1"
    , className =? "st-256color"    --> doShift "1"
    , className =? "Emacs"          --> doShift "1"
    , className =? "firefox"        --> doShift "2"
    , className =? "Chromium"       --> doShift "3"
    , className =? "Thunderbird"    --> doShift "4"
    , className =? "Signal"         --> doShift "8"
    , className =? "Slack"          --> doShift "9"
    , title =? "Dbeaver"            --> doShift "4"
    , className =? "DBeaver"        --> doShift "4"
    , title =? "Quit and close tabs?" --> doIgnore
    , resource  =? "desktop_window" --> doIgnore ]

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
            "feh --no-fehbg --bg-scale /home/jonathan/Pictures/wallpaper.jpg"
           , "xset -dpms"
           , "xset s off"
           , "picom -b"
           , "/usr/bin/xscreensaver -no-splash"
           , "emacs --bg-daemon"
           , "xset r rate 250 25"
           , "xset b off"
           , "dropbox-cli start"
           , "redshift"
           , "/home/jonathan/.local/bin/setmouse"
           , "/home/jonathan/.local/bin/tmuxcopy"
           , "dunst"
           ]
myStartupHook = do
    forM commands (\c -> spawnOnce c )
    nScreens <- countScreens
    forM [1..nScreens ] (\sc -> spawnOnce ("/home/jonathan/.local/bin/statusloop " ++ show sc))
    spawnOnce "xsetroot -cursor_name left_ptr"
------------------------------------------------------------------------
    -- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

   -- launch a terminal
   [ ((modm .|. shiftMask, xK_Return), spawn "emacsclient -c ~/")

   -- launch dmenu
     , ((controlMask,               xK_space     ), spawn "dmenu_run")

   -- close focused window
     , ((modm              , xK_q     ), kill)

    -- Rotate through the available layout algorithms
     , ((mod1Mask,               xK_space ), sendMessage NextLayout)

   --  Reset the layouts on the current workspace to default
     , ((mod1Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

   -- Resize viewed windows to the correct size
     , ((modm,               xK_n     ), refresh)

   -- Move focus to the next window
     , ((modm,               xK_Tab   ), windows W.focusDown)

   -- Move focus to the next window
     , ((modm,               xK_j     ), windows W.focusDown)

   -- Move focus to the previous window
     , ((modm,               xK_k     ), windows W.focusUp  )

   -- Move focus to the master window
     , ((modm,               xK_m     ), windows W.focusMaster  )

   -- Swap the focused window and the master window
     , ((modm,               xK_space), windows W.swapMaster)

   -- Swap the focused window with the next window
     , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

   -- Swap the focused window with the previous window
     , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

   -- Shrink the master area
     , ((modm,               xK_h     ), sendMessage Shrink)

   -- Expand the master area
     , ((modm,               xK_l     ), sendMessage Expand)

   -- Push window back into tiling
     , ((modm,               xK_t     ), withFocused $ windows . W.sink)

   -- Increment the number of windows in the master area
     , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

   -- Deincrement the number of windows in the master area
     , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

   -- Toggle the status bar gap
   -- Use this binding with avoidStruts from Hooks.ManageDocks.
   -- See also the statusBar function from Hooks.DynamicLog.
   --
     , ((modm              , xK_b     ), sendMessage ToggleStruts)

   -- Quit xmonad
     , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

   -- Restart xmonad
     , ((modm .|. controlMask, xK_q   ), spawn "xmonad --recompile; xmonad --restart")

   -- Run xmessage with a summary of the default keybindings (useful for beginners)
   ]
   ++

   --
   -- mod-[1..9], Switch to workspace N
   -- mod-shift-[1..9], Move client to workspace N
   --
   [((m .|. modm, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
   ++

   --
   -- mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3
   -- mod-shift-{a,s,d}, Move client to screen 1, 2, or 3
   --
   [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
------------------------------------------------------------------------
    -- Now run xmonad with all the defaults we set up.

main = do
    let dzncmd = "dzen2 -dock -ta l -fn mono:size=10 -y 1920 -xs  "
    nScreens <- countScreens
    handles <- forM [1..nScreens] (\sc -> spawnPipe (dzncmd ++ show sc))
    xmonad  $ withUrgencyHook NoUrgencyHook $ docks def
        { terminal           = myTerminal
          , focusFollowsMouse  = myFocusFollowsMouse
          , borderWidth        = myBorderWidth
          , manageHook         = manageDocks <+> myManageHook
          , layoutHook         = avoidStruts $ myLayout
          , modMask            = myModMask
          , logHook            = dynamicLogWithPP def
              { ppOutput       = \str -> forM_ handles (flip hPutStrLn str)
          , ppExtras       = [ windowCount ]
          , ppCurrent      = dzenColor "grey" "" . wrap "[" "]"
          , ppVisible      = dzenColor "grey" ""
          , ppHidden       = dzenColor "grey" ""
          , ppTitle        = dzenColor "grey" "" . shorten 550
          , ppLayout       = dzenColor "grey" "" . shorten 50
          , ppUrgent       = dzenColor "red" "" . shorten 50 . dzenStrip
          , ppOrder = \(a:b:c:d) ->  d ++ [ a ] ++  [ b ] ++  [ c ]
              }
          , keys               = myKeys
          , startupHook        = myStartupHook
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
          , ((0, xK_Print), spawn "~/.config/dwm/scripts/screenshot")
          , ((0, xK_Pause), spawn "~/bin/dnd")
          , ((shiftMask, xK_F12), spawn "systemctl poweroff")
          , ((0, xK_F12), spawn "systemctl suspend")
          , ((mod4Mask, xK_m), spawn "firefox --new-tab about:blank")
          , ((mod4Mask .|. shiftMask, xK_m), spawn "firefox --private-window")
          , ((mod4Mask .|. shiftMask, xK_g), spawn "chromium --incognito")
          , ((mod4Mask              , xK_Return     ), spawn myTerminal)
          , (( 0, xF86XK_AudioLowerVolume  ), spawn  "amixer -c 0 -- set Master 1-")
          , (( 0, xF86XK_AudioRaiseVolume  ), spawn "amixer -c 0 -- set Master 1+")
          , (( 0, xF86XK_MonBrightnessUp   ), spawn "xbacklight -inc 2")
          , (( 0, xF86XK_MonBrightnessDown ), spawn "xbacklight -dec 2")
        ]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
-- defaults = def {
--       -- simple stuff
--         terminal           = myTerminal,
--         focusFollowsMouse  = myFocusFollowsMouse,
--         clickJustFocuses   = myClickJustFocuses,
--         borderWidth        = myBorderWidth,
--         modMask            = myModMask,
--         workspaces         = myWorkspaces,
--         normalBorderColor  = myNormalBorderColor,
--         focusedBorderColor = myFocusedBorderColor,

--       -- key bindings
        -- keys               = myKeys,
--         mouseBindings      = myMouseBindings,

--       -- hooks, layouts
--         layoutHook         = myLayout,
--         manageHook         = manageDocks <+> myManageHook
--         -- handleEventHook    = myEventHook,

--         -- startupHook        = myStartupHook
--                }

-- -- | Finally, a copy of the default bindings in simple textual tabular format.

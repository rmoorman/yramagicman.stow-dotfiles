import Control.Monad
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.IndependentScreens
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

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
myLayout = avoidStruts $ tiled |||  Full ||| Mirror tiled ||| simpleFloat
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
      className =? "MPlayer"        --> doFloat
    , className =? "Firefox"        --> doShift "2"
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

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
-- myStartupHook = do
--     nScreens <- countScreens
--     withScreens nScreens spawnOnce "dzen2 -dock -xs 0 -ta l -w 1500 -fn xft:font=Inconsolata:size=10"
-- let
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    let dzncmd = "dzen2 -dock -ta l -w 1500 -fn mono:size=10 -xs  "
    nScreens <- countScreens
    handles <- forM [0..nScreens -1] (\sc -> spawnPipe (dzncmd ++ show sc))
    xmonad  $ docks def
        {
        terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        -- clickJustFocuses   = myClickJustFocuses
        , borderWidth        = myBorderWidth
        , manageHook         = manageDocks <+> myManageHook
        , layoutHook = myLayout
        , modMask    = myModMask
        , logHook    = dynamicLogWithPP defaultPP
            { ppOutput = \str -> forM_ handles (flip hPutStrLn str)
            , ppCurrent = dzenColor "grey" "" . wrap "[" "]"
            , ppVisible =  dzenColor "grey" ""
            , ppHidden =  dzenColor "grey" ""
            , ppTitle = dzenColor "grey" "" . shorten 50
            , ppLayout = dzenColor "grey" "" . shorten 50
            , ppUrgent = dzenColor "red" "" . wrap "*" "*"
            }

        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
          , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
          , ((0, xK_Print), spawn "scrot")
          , ((shiftMask, xK_F12), spawn "systemctl poweroff")
          , ((mod4Mask, xK_m), spawn "firefox --new-tab about:blank")
          , ((mod4Mask, xK_q     ), kill)
          , ((mod4Mask    .|. controlMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")
          , ((mod4Mask              , xK_b     ), sendMessage ToggleStruts)
          , ((mod4Mask              , xK_space     ), windows W.swapMaster)
          , ((mod1Mask              , xK_space ), sendMessage NextLayout)
          , ((controlMask           , xK_space ), spawn "dmenu_run")
          , ((mod4Mask              , xK_Return     ), spawn myTerminal)
          -- , ((mod1Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
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
--         -- keys               = myKeys,
--         mouseBindings      = myMouseBindings,

--       -- hooks, layouts
--         layoutHook         = myLayout,
--         manageHook         = manageDocks <+> myManageHook
--         -- handleEventHook    = myEventHook,

--         -- startupHook        = myStartupHook
--                }

-- -- | Finally, a copy of the default bindings in simple textual tabular format.

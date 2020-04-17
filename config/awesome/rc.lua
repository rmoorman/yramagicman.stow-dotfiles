-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors })
end
-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
        title = "Oops, an error happened!",
        text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
beautiful.useless_gap = 0
beautiful.font = 'Inconsolata'
beautiful.wallpaper = ''

-- This is used later as the default terminal and editor to run.
terminal = gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/termcmd'
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    -- awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu

mymainmenu = awful.menu({ })

-- mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
-- menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar
-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
awful.button({ }, 1, function(t) t:view_only() end),
awful.button({ modkey }, 1, function(t)
    if client.focus then
        client.focus:move_to_tag(t)
    end
end),
awful.button({ }, 3, awful.tag.viewtoggle),
awful.button({ modkey }, 3, function(t)
    if client.focus then
        client.focus:toggle_tag(t)
    end
end),
awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
awful.button({ }, 1, function (c)
    if c == client.focus then
        c.minimized = true
    else
        c:emit_signal(
        "request::activate",
        "tasklist",
        {raise = true}
        )
    end
end),
awful.button({ }, 3, function()
    awful.menu.client_list({ theme = { width = 250 } })
end),
awful.button({ }, 4, function ()
    awful.client.focus.byidx(1)
end),
awful.button({ }, 5, function ()
    awful.client.focus.byidx(-1)
end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }
    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        -- buttons = tasklist_buttons
        widget_template = {
            {
                {
                    {
                        id     = 'text_role',
                        widget = wibox.widget.textbox,
                    },
                    layout = wibox.layout.fixed.horizontal,
                },
                left  = 10,
                right = 10,
                widget = wibox.container.margin
            },
            id     = 'background_role',
            widget = wibox.container.background,
        },
    }


    mywidget = wibox.widget{
        text = '',
        align  = 'center',
        valign = 'center',
        widget = wibox.widget.textbox,
        screen = s,
    }
    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })
    if awful.screen.focused() then
        watchwidget = awful.widget.watch('/home/jonathan/bin/statusline', 1)
    else
        watchwidget =wibox.widget({text = '' })
    end
    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        mylauncher,
        s.mytaglist,
        s.mylayoutbox,
        s.mypromptbox,
    },
    s.mytasklist, -- Middle widget
    { -- Right widgets
    layout = wibox.layout.fixed.horizontal,
    -- wibox.widget.systray(),
    -- mytextclock,
    -- awful.widget.watch('date', 1)
    mywidget,
    watchwidget
},
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(

awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
{description = "go back", group = "tag"}),

awful.key({ modkey,           }, "j", function () awful.client.focus.byidx( 1) end,
{description = "focus next by index", group = "client"}),

awful.key({ modkey,           }, "k", function () awful.client.focus.byidx(-1) end,
{description = "focus previous by index", group = "client"}),

-- Layout manipulation

awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
{description = "swap with next client by index", group = "client"}),

awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
{description = "swap with previous client by index", group = "client"}),

awful.key({ modkey,           }, "l", function () awful.screen.focus_relative( 1) end,
{description = "focus the next screen", group = "screen"}),

awful.key({ modkey,           }, "h", function () awful.screen.focus_relative(-1) end,
{description = "focus the previous screen", group = "screen"}),

awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
{description = "jump to urgent client", group = "client"}),

awful.key({ modkey,           }, "t", function () awful.layout.set( awful.layout.suit.max )  end,
{description = "Tile windows", group = "layout"}),

awful.key({ modkey,           }, "e", function () awful.layout.set( awful.layout.suit.tile )  end,
{description = "maximize windows", group = "layout"}),

awful.key({ modkey,  "Control" }, "f", function () awful.layout.set( awful.layout.suit.floating )  end,
{description = "maximize windows", group = "layout"}),
awful.key({ modkey,           }, "Tab",
function ()
    awful.client.focus.history.previous()
    if client.focus then
        client.focus:raise()
    end
end,
{description = "go back", group = "client"}),

-- Standard program


awful.key({ modkey, "Shift" }, "r", awesome.restart,
{description = "reload awesome", group = "awesome"}),

awful.key({ modkey, "Shift"   }, "q", awesome.quit,
{description = "quit awesome", group = "awesome"}),

awful.key({ modkey, "Control" }, "l",     function () awful.tag.incmwfact( 0.05)          end,
{description = "increase master width factor", group = "layout"}),

awful.key({ modkey,   "Control" }, "h",     function () awful.tag.incmwfact(-0.05)          end,
{description = "decrease master width factor", group = "layout"}),

awful.key({ modkey, "Mod1"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
{description = "increase the number of master clients", group = "layout"}),

awful.key({ modkey, "Mod1"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
{description = "decrease the number of master clients", group = "layout"}),

awful.key({ modkey, "Control", "Shift" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
{description = "increase the number of columns", group = "layout"}),

awful.key({ modkey, "Control" , "Shift" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
{description = "decrease the number of columns", group = "layout"}),

-- Menubar
awful.key({ "Control" }, "space", function() menubar.show() end,
{description = "show the menubar", group = "launcher"}),

awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
{description = "open a terminal", group = "launcher"}),

awful.key({ modkey,           }, "m", function () awful.spawn("firefox --new-tab https://start.duckduckgo.com/lite") end,
{description = "open firefox", group = "launcher"}),

awful.key({ modkey,   "Shift" }, "m", function () awful.spawn("firefox --private-window") end,
{description = "open firefox", group = "launcher"}),

awful.key({ modkey,           }, "g", function () awful.spawn("chromium --force-dark-mode") end,
{description = "open chromium", group = "launcher"}),

awful.key({ modkey,   "Shift" }, "g", function () awful.spawn("chromium --force-dark-mode --incognito") end,
{description = "open chromium incognito", group = "launcher"}),

awful.key({                   }, "F12", function () awful.spawn("systemctl suspend") end,
{description = "suspend computer", group = "launcher"}),

awful.key({ "Shift",          }, "F12", function () awful.spawn("systemctl poweroff") end,
{description = "poweroff", group = "launcher"}),

awful.key({ "Shift",  "Control" }, "F12", function () awful.spawn("systemctl reboot") end,
{description = "poweroff", group = "launcher"}),

awful.key({                 }, "Print", function () awful.spawn(gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/screenshot') end,
{description = "open firefox", group = "launcher"}),

awful.key({}, "XF86AudioMute", function () awful.spawn( "amixer -c 0  --  set  Master  0" ) end,
{description = "mute audio", group = "launcher"}),

awful.key({}, "XF86AudioLowerVolume", function () awful.spawn( "amixer -c 0  --  set  Master  1-" ) end,
{description = "lower volume", group = "launcher"}),

awful.key({}, "XF86AudioRaiseVolume", function () awful.spawn( "amixer -c 0  --  set  Master  1+" ) end,
{description = "raise volume", group = "launcher"}),

awful.key({}, "XF86MonBrightnessUp",    function() awful.spawn(gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/brightup') end,
{description = "brighter", group = "launcher"}),

awful.key({}, "XF86MonBrightnessDown",    function() awful.spawn(gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/brightdown') end,
{description = "dimmer", group = "launcher"})
)

clientkeys = gears.table.join(

awful.key({ modkey,           }, "f",
function (c)
    c.fullscreen = not c.fullscreen
    c:raise()
end,
{description = "toggle fullscreen", group = "client"}),

awful.key({ modkey, }, "q",      function (c) c:kill()                         end,
{description = "close", group = "client"}),

awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
{description = "toggle floating", group = "client"}),

awful.key({ modkey, }, "space", function (c)
    if awful.client.focus then
        client.focus.swap(awful.client.getmaster(), client.focus)
    end
end,
{description = "move to master", group = "client"}),

awful.key({ modkey,     "Shift"      }, "l",      function (c) c:move_to_screen( c.screen.index + 1 )               end,
{description = "move to screen", group = "client"}),

awful.key({ modkey,     "Shift"      }, "h",      function (c) c:move_to_screen( c.screen.index - 1 )               end,
{description = "move to screen", group = "client"}),
awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
{description = "toggle keep on top", group = "client"}),

awful.key({ modkey,           }, "n",
function (c)
    -- The client currently has the input focus, so it cannot be
    -- minimized, since minimized clients can't have the focus.
    c.minimized = true
end ,
{description = "minimize", group = "client"})

)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
    -- View tag only.
    awful.key({ modkey }, "#" .. i + 9,
    function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
            tag:view_only()
        end
    end,
    {description = "view tag #"..i, group = "tag"}),
    -- Toggle tag display.
    awful.key({ modkey, "Control" }, "#" .. i + 9,
    function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
            awful.tag.viewtoggle(tag)
        end
    end,
    {description = "toggle tag #" .. i, group = "tag"}),
    -- Move client to tag.

    awful.key({ modkey, "Shift" }, "#" .. i + 9,
    function ()
        if client.focus then
            local tag = client.focus.screen.tags[i]
            if tag then
                client.focus:move_to_tag(tag)
            end
        end
    end,
    {description = "move focused client to tag #"..i, group = "tag"}),

    -- Toggle tag on focused client.
    awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
    function ()
        if client.focus then
            local tag = client.focus.screen.tags[i]
            if tag then
                client.focus:toggle_tag(tag)
            end
        end
    end,
    {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
awful.button({ }, 1, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
end),
awful.button({ modkey }, 1, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.move(c)
end),
awful.button({ modkey }, 3, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.resize(c)
end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
    properties = { border_width = beautiful.border_width,
    border_color = beautiful.border_normal,
    focus = awful.client.focus.filter,
    raise = true,
    keys = clientkeys,
    buttons = clientbuttons,
    screen = awful.screen.preferred,
    placement = awful.placement.no_overlap+awful.placement.no_offscreen
}
    },

    -- Floating clients.
    { rule_any = {
        instance = {
            "DTA",  -- Firefox addon DownThemAll.
            "copyq",  -- Includes session name in class.
            "pinentry",
        },
        class = {
            "Arandr",
            "Blueman-manager",
            "Gpick",
            "Kruler",
            "MessageWin",  -- kalarm.
            "Sxiv",
            "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
            "Wpa_gui",
            "veromix",
            "xtightvncviewer"},

            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester",  -- xev.
            },
            role = {
                "AlarmWindow",  -- Thunderbird's calendar.
                "ConfigManager",  -- Thunderbird's about:config.
                "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
            }
        }, properties = { floating = true }},

        -- Add titlebars to normal clients and dialogs
        { rule_any = {type = { "normal", "dialog" }
    }, properties = { titlebars_enabled = true }
},
}
function one()
    local rules = {
        { rule_any = { class = { "Firefox", "firefox", "Alacritty" } },
        properties = { screen = 1, tag = "1" } },
        { rule_any = { class = { "Chromium", "chromium" } },
        properties = { screen = 1, tag = "2" } },
        { rule_any = { class = { "Thunderbird", "thunderbird" } },
        properties = { screen = 1, tag = "3" } },
        { rule_any = { class = { "Signal", "signal" } },
        properties = { screen = 1, tag = "9" } },
        { rule_any = { class = { "Krita", "krita" } },
        properties = { screen = 1, tag = "4" } },
    }
    for r = 1,#rules do
        table.insert(awful.rules.rules, rules[r])
    end
end

function two()
    local rules = {

        { rule_any = { class = { "Alacritty" } },
        properties = { screen = 2, tag = "1" } },
        { rule_any = { class = { "Firefox", "firefox" } },
        properties = { screen = 1, tag = "1" } },
        { rule_any = { class = { "Chromium", "chromium" } },
        properties = { screen = 1, tag = "2" } },
        { rule_any = { class = { "Thunderbird", "thunderbird" } },
        properties = { screen = 1, tag = "3" } },
        { rule_any = { class = { "Signal", "signal" } },
        properties = { screen = 1, tag = "9" } },
        { rule_any = { class = { "Krita", "krita" } },
        properties = { screen = 1, tag = "4" } },
    }
    for r = 1,#rules do
        table.insert(awful.rules.rules, rules[r])
    end
end

if screen.count() == 1 then
    naughty.notify({ title = "Monitors", text = 'one monitor' })
    one()
elseif screen.count() == 2 then
    naughty.notify({ title = "Monitors", text = 'two monitors' })
    two()
end
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
        and not c.size_hints.user_position
        and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

awful.spawn.with_shell("~/.config/dwm/autostart.sh")

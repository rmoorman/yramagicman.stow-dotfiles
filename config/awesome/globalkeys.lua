local gears = require("gears")
local awful = require("awful")
local menubar = require("menubar")

local globalkeys = {}
function globalkeys.keys(modkey)
    return gears.table.join(
    -- {{{ focus
    awful.key({ modkey, }, "Escape", awful.tag.history.restore,
    {description = "go back", group = "tag"}),

    awful.key({ modkey, }, "j", function () awful.client.focus.byidx( 1) end,
    {description = "focus next by index", group = "client"}),

    awful.key({ modkey, }, "k", function () awful.client.focus.byidx(-1) end,
    {description = "focus previous by index", group = "client"}),

    awful.key({ modkey, }, "h", function () awful.screen.focus_relative(-1) end,
    {description = "focus the previous screen", group = "screen"}),

    awful.key({ modkey, }, "l", function () awful.screen.focus_relative( 1) end,
    {description = "focus the next screen", group = "screen"}),

    awful.key({ modkey, }, "u", awful.client.urgent.jumpto,
    {description = "jump to urgent client", group = "client"}),

    -- }}}
    -- {{{ Layout manipulation
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx( 1) end,
    {description = "swap with next client by index", group = "client"}),

    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx( -1) end,
    {description = "swap with previous client by index", group = "client"}),



    awful.key({ modkey, }, "t", function () awful.layout.set( awful.layout.suit.max ) end,
    {description = "Tile windows", group = "layout"}),

    awful.key({ modkey, }, "e", function () awful.layout.set( awful.layout.suit.tile ) end,
    {description = "Tile windows", group = "layout"}),

    awful.key({ modkey, "Control" }, "f", function () awful.layout.set( awful.layout.suit.floating ) end,
    {description = "Floating layout", group = "layout"}),

    awful.key({ modkey, }, "Tab",
    function ()
        awful.client.focus.history.previous()
        if client.focus then
            client.focus:raise()
        end
    end,
    {description = "go back", group = "client"}),
    -- }}}
    --{{{ Standard program

    awful.key({ modkey, "Shift" }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),

    awful.key({ modkey, "Shift" }, "q", awesome.quit,
    {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey, "Control" }, "l", function () awful.tag.incmwfact( 0.05) end,
    {description = "increase master width factor", group = "layout"}),

    awful.key({ modkey, "Control" }, "h", function () awful.tag.incmwfact(-0.05) end,
    {description = "decrease master width factor", group = "layout"}),

    awful.key({ modkey, "Mod1" }, "h", function () awful.tag.incnmaster(-1, nil, true) end,
    {description = "increase the number of master clients", group = "layout"}),

    awful.key({ modkey, "Mod1" }, "l", function () awful.tag.incnmaster( 1, nil, true) end,
    {description = "decrease the number of master clients", group = "layout"}),

    awful.key({ modkey, "Control", "Shift" }, "h", function () awful.tag.incncol( 1, nil, true) end,
    {description = "increase the number of columns", group = "layout"}),

    awful.key({ modkey, "Control" , "Shift" }, "l", function () awful.tag.incncol(-1, nil, true) end,
    {description = "decrease the number of columns", group = "layout"}),
    -- }}}
    -- {{{ Menubar
    awful.key({ "Control" }, "space", function() menubar.show() end,
    {description = "show the menubar", group = "launcher"}),

    awful.key({ modkey, }, "Return", function () awful.spawn(terminal) end,
    {description = "open a terminal", group = "launcher"}),

    awful.key({ modkey, "Shift" }, "Return", function () awful.spawn("emacs ~/") end,
    {description = "open a terminal", group = "launcher"}),
    --}}}
    -- {{{ Program keys
    awful.key({ modkey, }, "m", function () awful.spawn("firefox --new-tab https://start.duckduckgo.com/lite") end,
    {description = "open firefox", group = "launcher"}),

    awful.key({ modkey, "Shift" }, "m", function () awful.spawn("firefox --private-window") end,
    {description = "open firefox", group = "launcher"}),

    awful.key({ modkey, }, "g", function () awful.spawn("chromium --force-dark-mode") end,
    {description = "open chromium", group = "launcher"}),

    awful.key({ modkey, "Shift" }, "g", function () awful.spawn("chromium --force-dark-mode --incognito") end,
    {description = "open chromium incognito", group = "launcher"}),

    awful.key({ }, "F12", function () awful.spawn("systemctl suspend") end,
    {description = "suspend computer", group = "launcher"}),

    awful.key({ "Shift", }, "F12", function () awful.spawn("systemctl poweroff") end,
    {description = "poweroff", group = "launcher"}),

    awful.key({ "Shift", "Control" }, "F12", function () awful.spawn("systemctl reboot") end,
    {description = "poweroff", group = "launcher"}),

    awful.key({ }, "Print", function () awful.spawn(gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/screenshot') end,
    {description = "open firefox", group = "launcher"}),

    awful.key({}, "XF86AudioMute", function () awful.spawn( "amixer -c 0 -- set Master 0" ) end,
    {description = "mute audio", group = "launcher"}),

    awful.key({}, "XF86AudioLowerVolume", function () awful.spawn( "amixer -c 0 -- set Master 1-" ) end,
    {description = "lower volume", group = "launcher"}),

    awful.key({}, "XF86AudioRaiseVolume", function () awful.spawn( "amixer -c 0 -- set Master 1+" ) end,
    {description = "raise volume", group = "launcher"}),

    awful.key({}, "XF86MonBrightnessUp", function() awful.spawn(gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/brightup') end,
    {description = "brighter", group = "launcher"}),

    awful.key({}, "XF86MonBrightnessDown", function() awful.spawn(gears.filesystem.get_xdg_config_home() .. 'dwm/scripts/brightdown') end,
    {description = "dimmer", group = "launcher"})

    --}}}
    )
end

function globalkeys.tagkeys(globalkeys, modkey, index)
    return gears.table.join(globalkeys,
    -- {{{ View tag only.
    awful.key({ modkey }, "#" .. index + 9,
    function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[index]
        if tag then
            tag:view_only()
        end
    end,
    {description = "view tag #".. index, group = "tag"}),
    -- }}}
    -- {{{ Toggle tag display.
    awful.key({ modkey, "Control" }, "#" .. index + 9,
    function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[index]
        if tag then
            awful.tag.viewtoggle(tag)
        end
    end,
    {description = "toggle tag #" .. index, group = "tag"}),
    -- }}}
    -- {{{ Move client to tag.
    awful.key({ modkey, "Shift" }, "#" .. index + 9,
    function ()
        if client.focus then
            local tag = client.focus.screen.tags[index]
            if tag then
                client.focus:move_to_tag(tag)
            end
        end
    end,
    {description = "move focused client to tag #".. index, group = "tag"}),
    -- }}}
    -- {{{ Toggle tag on focused client.
    awful.key({ modkey, "Control", "Shift" }, "#" .. index + 9,
    function ()
        if client.focus then
            local tag = client.focus.screen.tags[index]
            if tag then
                client.focus:toggle_tag(tag)
            end
        end
    end,
    {description = "toggle focused client on tag #" .. index, group = "tag"})
    -- }}}
    )
end
return globalkeys

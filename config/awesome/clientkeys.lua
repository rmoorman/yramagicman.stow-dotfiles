local gears = require("gears")
local awful = require("awful")
local clientkeys = {}
function clientkeys.keys(modkey)
return gears.table.join(

    awful.key({ modkey, }, "f",
    function (c)
        c.fullscreen = not c.fullscreen
        c:raise()
    end,
    {description = "toggle fullscreen", group = "client"}),

    awful.key({ modkey, }, "q", function (c) c:kill() end,
    {description = "close", group = "client"}),

    awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle ,
    {description = "toggle floating", group = "client"}),

    awful.key({ modkey, }, "space", function (c)
        if awful.client.focus then
            client.focus.swap(awful.client.getmaster(), client.focus)
        end
    end,
    {description = "move to master", group = "client"}),

    awful.key({ modkey, "Shift" }, "l", function (c) c:move_to_screen( c.screen.index + 1 ) end,
    {description = "move to screen", group = "client"}),

    awful.key({ modkey, "Shift" }, "h", function (c) c:move_to_screen( c.screen.index - 1 ) end,
    {description = "move to screen", group = "client"}),
    awful.key({ modkey, "Control", "Shift" }, "t", function (c) c.ontop = not c.ontop end,
    {description = "toggle keep on top", group = "client"}),

    awful.key({ modkey, }, "n",
    function (c)
        -- The client currently has the input focus, so it cannot be
        -- minimized, since minimized clients can't have the focus.
        c.minimized = true
    end ,
    {description = "minimize", group = "client"})

)
end
return clientkeys

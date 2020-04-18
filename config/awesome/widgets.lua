local awful = require("awful")
local wibox = require("wibox")
widgets = {}
function widgets.blanktext ()
    wibox.widget{
        text = '',
        align  = 'center',
        valign = 'center',
        widget = wibox.widget.textbox,
        screen = s,
    }
end

function widgets.date(format)
    awful.widget.watch('date +"'.. format ..'"', 1)
end
return widgets

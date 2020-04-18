local gizmos = {}
local awful = require("awful")
local wibox = require("wibox")
local buttons = require("buttons")

function gizmos.blanktext ()
    return wibox.widget{
        text = '',
        align  = 'center',
        valign = 'center',
        widget = wibox.widget.textbox,
        screen = s,
    }
end

function gizmos.showtext (t)
    return wibox.widget {
        text = t,
        align  = 'center',
        valign = 'center',
        widget = wibox.widget.textbox,
        screen = s,
    }
end

function gizmos.sep(width)
    return wibox.widget {
        widget = wibox.widget.separator,
        forced_width = width,
        opacity=0
    }
end

function gizmos.update()
    return awful.widget.watch("bash -c 'cat /home/jonathan/.cache/updates | wc -l'", 10, function (w, out)
        if tonumber( out ) > 15 then
            w:set_text( 'U: ' .. tostring(out) )
        end
    end)
end

function gizmos.batcap()
    return awful.widget.watch("bash -c 'cat /sys/class/power_supply/BAT?/capacity'", 1, function (w, out)
        local outStr = tostring(out)
        local trimmed = outStr:gsub("%s+", "")
        w:set_text( 'B: ' .. trimmed .. '%' )
    end)
end

function gizmos.batstat()
    return awful.widget.watch("bash -c 'cat /sys/class/power_supply/BAT?/status'", 30, function (w, out)
        local outStr = tostring(out)
        local trimmed = outStr:gsub("%s+", "")
        local indicator=""
        if trimmed == 'Charging' then
            indicator="\xE2\x86\x91"
        elseif trimmed == 'Full' then
            indicator="\xe2\x9c\x93"
        else
            indicator="\xE2\x86\x93"
        end
        w:set_text( indicator )
    end)
end

function gizmos.dbox()
    return awful.widget.watch(" bash -c 'dropbox-cli status' ", 1, function (w, out)
        local message = ""
        if out == "Dropbox isn't running!" then
            message="off"
        else
            message = out:gsub("%s+$", "")
        end
        if message == "Up to date" then
            message="\xe2\x9c\x93"
        elseif message:find('Syncing') then
            message = message
        end
        w:set_text('D: ' .. message)
    end)
end

function gizmos.date(format)
    return awful.widget.watch('date +"'.. format ..'"', 1)
end

function gizmos.taglist(s)
    return awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons =buttons.taglist()
    }
end

function gizmos.tasklist(s)
    return awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = '', -- tasklist_buttons
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
end

return gizmos

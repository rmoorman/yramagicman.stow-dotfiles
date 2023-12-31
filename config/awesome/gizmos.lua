local gizmos = {}
local awful = require("awful")
local wibox = require("wibox")
local buttons = require("buttons")
local naughty = require("naughty")

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

function gizmos.update(prefix, suffix, mincount)
    if prefix == nil then
        prefix = ''
    end
    if suffix == nil then
        suffix = ''
    end
    return awful.widget.watch("bash -c 'cat ~/.cache/updates ~/.cache/aur | wc -l'", 1, function (w, out)
        if tonumber( out ) > mincount then
            message= prefix..out .. suffix
        else
            message= prefix.."\xe2\x9c\x93"..suffix
        end
        w:set_text(message)
    end)
end

function gizmos.aur(prefix, suffix)
    if prefix == nil then
        prefix = ''
    end
    if suffix == nil then
        suffix = ''
    end
    return awful.widget.watch("bash -c 'cat ~/.cache/aur | wc -l'", 1, function (w, out)
        if tonumber(out) > 0 then
            w:set_text( prefix .. tostring(out) .. suffix)
        end
    end)
end

function gizmos.batcap(prefix, suffix)
    if prefix == nil then
        prefix = ''
    end
    if suffix == nil then
        suffix = ''
    end
    local cap
    return awful.widget.watch("bash -c 'cat /sys/class/power_supply/BAT?/capacity'", 60, function (w, out)
        local outStr = tostring(out)
        local trimmed = outStr:gsub("%s+", "")
        if tonumber(trimmed) == 100 then
            w:set_text( prefix )
        else
            w:set_text( prefix .. trimmed .. suffix)
        end
        if outStr == '' then
            w:set_text('')
        end
        if tonumber( out ) < 20 then
            gizmos.changeNotify(cap, out, "Battery Level", out)
        end
        cap = out
    end)
end

function gizmos.batstat(prefix, suffix)
    if prefix == nil then
        prefix = ''
    end
    if suffix == nil then
        suffix = ''
    end
    local stat
    return awful.widget.watch("bash -c 'cat /sys/class/power_supply/BAT?/status'", 1, function (w, out)
        local outStr = tostring(out)
        local trimmed = outStr:gsub("%s+", "")
        local indicator=""
        if trimmed == '' then
            indicator=""
        elseif trimmed == 'Charging' then
            indicator="\xE2\x86\x91"
            gizmos.changeNotify(stat, out, "Battery Status", "Charging")
            stat = out
        elseif trimmed == 'Full' then
            indicator="\xe2\x9c\x93"
            gizmos.changeNotify(stat, out, "Battery Status", "Full")
            stat = out
        else
            indicator="\xE2\x86\x93"
            gizmos.changeNotify(stat, out, "Battery Status", "Discharging")
            stat = out
        end
        w:set_text( prefix..indicator..suffix )
        if trimmed == '' then
            w:set_text('')
        end
    end)
end

function gizmos.ip(prefix, suffix)
    if prefix == nil then
        prefix = ''
    end
    if suffix == nil then
        suffix = ''
    end
    return awful.widget.watch("ip --color=never addr show", 60, function(w, out)
            local ip = out:match('%d+%.%d+%.%d+%.%d+/%d+ brd')
            w:set_text(prefix.. ip:gsub("%s+brd", "") .. suffix)
        end)
    end

    function gizmos.ssid(wlan, prefix, suffix)
        if prefix == nil then
            prefix = ''
        end
        if suffix == nil then
            suffix = ''
        end
        return awful.widget.watch("iwctl station ".. wlan .."  show", 10, function(w, out)
            local wlan = out:match('Connected network%s+%g+')
            if wlan then
                if awful.screen.focused() then
                    w:set_text(prefix .. wlan:gsub('Connected network%s+', '') .. suffix)
                end
            end
        end)
    end

    function gizmos.changeNotify(cond1, cond2, title, text )
        if cond1 ~= cond2 then
            naughty.notify({
                    title = title,
                text = text })
        end
    end

    function gizmos.volume(prefix, suffix, showdb)
        if prefix == nil then
            prefix = ''
        end
        if suffix == nil then
            suffix = ''
        end
        if showdb == nil then
            showdb = false
        end
        return awful.widget.watch("bash -c 'amixer -c 0  -- get Master'", 1, function (w, out)
                local outStr = tostring(out)
                trimmed = outStr:gsub('%s+', '@')
                index = trimmed:find('Mono:')
                if  index > 0 then
                    local percent = trimmed:sub(index)
                    percent = percent:gsub('Mono:@Playback@%d+@', '')
                    percent = percent:gsub('@', '')
                    if showdb == false then
                        percent = percent:gsub('%[--?%d+.%d%ddB%]', '')
                    end
                    w:set_text( prefix .. percent .. suffix)
                end
            end)
        end


        function gizmos.date(format, prefix, suffix)
            if prefix == nil then
                prefix = ''
            end
            if suffix == nil then
                suffix = ''
            end
            return awful.widget.watch('date +"'.. format ..'"', 1, function(w, out)
                out = out:gsub('\n','')
                local message = prefix .. out .. suffix
                w:set_text(message)
            end)
        end

        function gizmos.mail(prefix, suffix)
            if prefix == nil then
                prefix = ''
            end
            if suffix == nil then
                suffix = ''
            end
            return awful.widget.watch('bash -c "ls ~/.config/mail/Inbox/new/ | wc -l"', 1, function(w, out)
                if tonumber( out ) > 0 then
                    local message = prefix .. out .. suffix
                    w:set_text(message)
                else
                    w:set_text('')
                end
            end)
        end

        function gizmos.time(prefix, suffix)
            if prefix == nil then
                prefix = ''
            end
            if suffix == nil then
                suffix = ''
            end
            minutes = 60 * 5
            return awful.widget.watch('bash -c "/home/jonathan/bin/harvest"',minutes , function(w, out)
                    local message = prefix .. out .. suffix
                    w:set_text(message)
            end)
        end

        function gizmos.taglist(s)
            return awful.widget.taglist {
                screen  = s,
                filter  = awful.widget.taglist.filter.all,
                buttons = buttons.taglist(),
            }
        end

        function gizmos.tasklist(s)
            return awful.widget.tasklist {
                screen  = s,
                filter  = awful.widget.tasklist.filter.currenttags,
                buttons = buttons.tasklist(),
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

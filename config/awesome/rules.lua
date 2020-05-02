local rules = {}
local awful = require("awful")
local naughty = require("naughty")

function rules.one()
    naughty.notify({ text = 'One monitor'})
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

function rules.two()
    naughty.notify({ text = 'Two monitors'})
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

function rules.three()
    naughty.notify({ text = 'Three monitors'})
    local rules = {
        { rule_any = { class = { "Alacritty" } },
        properties = { screen = 1, tag = "1" } },
        { rule_any = { class = { "Firefox", "firefox" } },
        properties = { screen = 2, tag = "1" } },
        { rule_any = { class = { "Chromium", "chromium" } },
        properties = { screen = 2, tag = "2" } },
        { rule_any = { class = { "Thunderbird", "thunderbird" } },
        properties = { screen = 2, tag = "3" } },
        { rule_any = { class = { "Signal", "signal" } },
        properties = { screen = 3, tag = "9" } },
        { rule_any = { class = { "Krita", "krita" } },
        properties = { screen = 1, tag = "4" } },
    }
    for r = 1,#rules do
        table.insert(awful.rules.rules, rules[r])
    end
end
return rules

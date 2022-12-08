normalTable = {
    "color0:",
    "color1:",
    "color2:",
    "color3:",
    "color4:",
    "color5:",
    "color6:",
    "color7:"
}

brightTable = {
    "color8:",
    "color9:",
    "color10:",
    "color11:",
    "color12:",
    "color13:",
    "color14:",
    "color15:",
}

foregroundTable = { "foreground:" }
backgroundTable = { "background:" }
cursorColorTable = { "cursorColor:" }

function  readXresourcesColors(colorTable, location)
    result = {}
    for l in io.lines(location)
    do
        for i = 1, #colorTable, 1
        do
            if l.find(l, colorTable[i]) then
                result[i] = string.gsub(l,"*."..colorTable[i] .."%s+", "")
            end
        end
    end
    return result
end

normal = readXresourcesColors(normalTable,"/home/jonathan/.config/X11/Xresources")
bright = readXresourcesColors(brightTable,"/home/jonathan/.config/X11/Xresources")
foreground = readXresourcesColors(foregroundTable,"/home/jonathan/.config/X11/Xresources")
background = readXresourcesColors(backgroundTable,"/home/jonathan/.config/X11/Xresources")
cursorColor = readXresourcesColors(cursorColorTable,"/home/jonathan/.config/X11/Xresources")

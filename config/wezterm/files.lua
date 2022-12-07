local normalTable = {
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


function  readXresourcesColors(colorTable, location)
    result = {}
    for l in io.lines(location)
    do
        for i = 1, table.maxn(colorTable), 1
        do
            if l.find(l, colorTable[i]) then
                result[i] = string.gsub(l,"*."..colorTable[i] .."%s+", "")
            end
        end
    end
    return result
end

local normal = readXresourcesColors(normalTable,"/home/jonathan/.config/X11/Xresources")



local bright = readXresourcesColors(brightTable,"/home/jonathan/.config/X11/Xresources")

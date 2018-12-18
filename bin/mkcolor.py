#!/usr/bin/python

import os
import sys

home = os.path.expanduser('~')

st_dir = home + '/Gits/st/'
dwm_dir = home + '/Gits/dwm/'
config_dir = home + '/Gits/dots/'
colors_dir = home + '/.colors/'

foreground = 'foreground'
background = 'background'
border = 'color0'
sel_foreground = 'color0'
sel_background = 'color1'
sel_border = 'color1'


def contains_color(str):
    if '.color' in str:
        return str


def not_contains_color(str):
    if '.color' not in str:
        return str


def not_contains_foreground(str):
    if '.foreground' not in str:
        return str


def not_contains_background(str):
    if '.background' not in str:
        return str


def not_contains_cursorColor(str):
    if '.cursorColor' not in str:
        return str


def not_contains_bang(str):
    if '!' not in str:
        return str


def read_color():
    with open(st_dir + 'config.h') as f:
        color = filter(contains_color, f.readlines())
        color = [c for c in color]
        color = color[0].strip()
        color = color.split('/')
        f.close()
        return color[-1][:-3]


def empty_xresources():
    xresources = ''
    with open(config_dir + 'Xresources') as f:
        x = []
        x = filter(not_contains_color, f.readlines())
        x = filter(not_contains_foreground, x)
        x = filter(not_contains_background, x)
        x = filter(not_contains_cursorColor, x)
        x = filter(not_contains_bang, x)
        xresources = xresources.join(x)
        f.close()
    try:
        os.remove('/tmp/xresources')
    except Exception:
        pass
    with open('/tmp/xresources', 'w+') as write:
        write.write(xresources)
    return xresources


def rebuild_colors(selection):
    with open(colors_dir + selection + '.xresources') as xr:
        colors = [color for color in xr.readlines()]
        i3 = ['i3wm.' + c for c in colors]
        f = open('/tmp/xresources', 'a+')
        f.write(''.join(i3))
        f.write(''.join(colors))
        f.close()


def list_colors():
    return set([f.split('.')[0] for f in os.listdir(colors_dir)])


def dwm_mod_color(line, color):
    print(line.split())


def dwm_update():
    with open(dwm_dir + 'config.h') as dwm:
        for line in dwm.readlines():
            if 'char background' in line:
                dwm_mod_color(line)
            if 'char sel_background' in line:
                dwm_mod_color(line)
            if 'char foreground' in line:
                dwm_mod_color(line)
            if 'char sel_foreground' in line:
                dwm_mod_color(line)
            if 'char border' in line:
                dwm_mod_color(line)
            if 'char sel_border' in line:
                dwm_mod_color(line)


if len(sys.argv) < 2:
    dwm_update()
    print('you are currently using ' + read_color())
    print('Possible arguments are:')
    for c in list_colors():
        print(c)
else:
    empty_xresources()
    rebuild_colors(sys.argv[-1])

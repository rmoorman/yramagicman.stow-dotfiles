#!/usr/bin/env python

import os
import re
import sys
from subprocess import call
from shutil import copyfile

home = os.path.expanduser('~')

st_dir = home + '/Documents/st/'
dwm_dir = home + '/Documents/dwm/'
config_dir = home + '/Documents/dots/'
colors_dir = config_dir + 'config/colors/'

foreground = 'foreground'
background = 'background'
border = 'color0'
sel_foreground = 'background'
sel_background = 'foreground'
sel_border = 'foreground'


def contains_color(str):
    if 'colors' in str:
        return str


def not_contains_color(str):
    if 'colors' not in str:
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


def not_blank(str):
    blanks = re.compile(r'\s')
    if not blanks.match(str):
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
    with open(config_dir + 'config/X11/Xresources') as f:
        x = []
        x = filter(not_contains_color, f.readlines())
        x = filter(not_contains_foreground, x)
        x = filter(not_contains_background, x)
        x = filter(not_contains_cursorColor, x)
        x = filter(not_contains_bang, x)
        x = filter(not_blank, x)
        xresources = xresources.join(x)
        f.close()
    try:
        os.remove('/tmp/xresources')
    except Exception:
        print('no previous xresources file compiled, continuing.')
    with open('/tmp/xresources', 'w+') as write:
        write.write(xresources)
    return xresources


def rebuild_colors(selection):
    with open(colors_dir + selection + '.xresources') as xr:
        colors = [color for color in xr.readlines()]
        f = open('/tmp/xresources', 'a+')
        f.write(''.join(colors))
        f.close()


def list_colors():
    return set([f.split('.')[0] for f in os.listdir(colors_dir)])


def find_color(selection, color):
    with open(colors_dir + selection + '.xresources') as xr:
        for line in xr.readlines():
            if color in line:
                return line.split()[-1]


def dwm_mod_color(line, selection, color):
    color = find_color(selection, color)
    new = line.split()
    new[-1] = '"' + color + '"' + ';\n'
    return ' '.join(new)


def set_bg(selection):
    color = find_color(selection, 'background')
    with open(config_dir + 'config/dwm/scripts/bg.sh', 'w') as dwm:
        write = ['#!/bin/sh',
                 'convert -size 100x100 xc:' + color + ' /tmp/bg.png\n'
                 '"$HOME/.config/fehbg"\n'
                 ]
        dwm.write('\n'.join(write))
    call(config_dir + 'config/dwm/scripts/bg.sh')
    call(home + '/.config/fehbg')


def dwm_update(selection):
    headerlines = []
    with open(dwm_dir + 'config.h') as dwm:
        for line in dwm.readlines():
            if 'char background' in line:
                line = dwm_mod_color(line, selection, background)
                headerlines.append(line)
            elif 'char sel_background' in line:
                line = dwm_mod_color(line, selection, sel_background)
                headerlines.append(line)
            elif 'char foreground' in line:
                line = dwm_mod_color(line, selection, foreground)
                headerlines.append(line)
            elif 'char sel_foreground' in line:
                line = dwm_mod_color(line, selection, sel_foreground)
                headerlines.append(line)
            elif 'char border' in line:
                line = dwm_mod_color(line, selection, border)
                headerlines.append(line)
            elif 'char sel_border' in line:
                line = dwm_mod_color(line, selection, sel_border)
                headerlines.append(line)
            else:
                headerlines.append(line)
        dwm.close()
    with open('/tmp/dwm.h', 'w+') as header:
        for line in headerlines:
            header.write(line)
        header.close()


def st_update(selection):
    config = []
    with open(st_dir + 'config.h') as st:
        for line in st.readlines():
            if '#include' in line:
                new = line.strip().split('/')
                new[-1] = selection + '.h"\n'
                print(new)
                exit
                config.append('/'.join(new))
            else:
                config.append(line)
        st.close()
    with open('/tmp/st.h', 'w+') as header:
        for line in config:
            header.write(line)
        header.close()


def move_into_place():
    errors = 0
    try:
        copyfile('/tmp/xresources', config_dir + 'config/X11/Xresources')
    except IOError:
        print("couldn't copy /tmp/xresources into place. Leaving it up to you")
        errors = errors + 1
    try:
        copyfile('/tmp/dwm.h', dwm_dir + 'config.h')
        copyfile('/tmp/dwm.h', config_dir + 'dwm.config.h')
    except IOError:
        print("couldn't copy /tmp/dwm.h into place. Leaving it up to you")
        errors = errors + 1
    try:
        copyfile('/tmp/st.h', st_dir + 'config.h')
        copyfile('/tmp/st.h', config_dir + 'st.config.h')
    except IOError:
        print("couldn't copy /tmp/st.h into place. Leaving it up to you")
        errors = errors + 1
    if errors > 0:
        exit()


def compile():
    errors = 0
    for dir in [st_dir, dwm_dir]:
        try:
            print()
            print('building ' + dir)
            os.chdir(dir)
            print()
            call('make')
        except Exception:
            errors = errors + 1
            print()
            print("couldn't make " + dir)
    if errors > 0:
        exit()
    os.chdir(home)
    call(['xrdb', '-merge', home + '/.config/X11/Xresources'])


def install():
    errors = 0
    for dir in [st_dir, dwm_dir]:
        try:
            print()
            print('installing ' + dir)
            os.chdir(dir)
            call(['/usr/bin/sudo', '/usr/bin/make', 'install'])
        except Exception:
            errors = errors + 1
            print()
            print("couldn't install " + dir)
    if errors > 0:
        exit()


if len(sys.argv) < 2:
    print('you are currently using ' + read_color())
    print('Possible arguments are:')
    for c in list_colors():
        print(c)
else:
    empty_xresources()
    rebuild_colors(sys.argv[-1])
    dwm_update(sys.argv[-1])
    st_update(sys.argv[-1])
    set_bg(sys.argv[-1])
    move_into_place()
    compile()
    install()
    call(['pkill', 'dwm'])

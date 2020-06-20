#!/usr/bin/env python3
import secrets
import sys

if len(sys.argv) < 2:
    print('need an argument')
    exit()

words = open('/usr/share/dict/cracklib-small')
words = words.readlines()
pw = []
for i in range(0, int(sys.argv[-1])):
    pw.append(secrets.choice(words).strip())

print(' '.join(pw).replace("'", ''))

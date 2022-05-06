#!/usr/bin/env python3
import secrets
import sys

if len(sys.argv) < 2:
    print('need an argument')
    print('arg 1: word list')
    print('arg 2: count')
    exit()

words = open(sys.argv[-2])
words = words.readlines()
pw = []
for i in range(0, int(sys.argv[-1])):
    pw.append(secrets.choice(words).strip())

print(' '.join(pw).replace("'", ''))

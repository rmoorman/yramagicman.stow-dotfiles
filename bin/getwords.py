#!/usr/bin/python
import sys
from random import seed, randint
seed()
dict = open('/usr/share/dict/cracklib-small')
words = dict.readlines()
words = map(lambda x: x.strip(), words)
words = [word for word in words]
if len(sys.argv) != 2:
    print('need number of words')
    exit()
ws = []
for i in range(0, int(sys.argv[1])):
    ws.append(words[randint(0, len(words))])

print(' '.join(ws))

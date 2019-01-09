import subprocess
lines = []
with open('accounts') as f:
    for l in f.readlines():
        lines.append(l)
    f.close()

count = 1
for l in lines:
    account = l.split(' ')
    name = account[0]
    pw = account[1]
    print(pw)
    print(count, 'of ', len(lines))
    subprocess.call(['pass', 'insert', name])
    print()
    count = count + 1

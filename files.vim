let files = split(globpath('./**', '*.vim'), '\n')
for file in files
    if index(split(file,'/'), 'undo') == -1
        execute 'source ' . file
    endif
endfor

autocmd bufwinenter,cursorhold * call findtags#Findtags()
command! FindTags :call findtags#Findtags()

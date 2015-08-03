" Vim configuration file
"
" Pathogen package management
" Packages this uses: vim-sensible, vim-surround, supertab, base16-vim
execute pathogen#infect()

" general settings
set nobackup	" do not keep a backup file, use versions instead
set viminfo='50,\"50 " read/write a .viminfo file
set showcmd    "show incomplete commands
set showmatch  "show matching parentheses and brackets and things
set showmode   "show the current mode (insert, etc)
set vb         "shut off the damn beeping
silent !mkdir -p ~/.vimswap
set directory=~/.vimswap//

set rulerformat=%55(%{strftime('%a\ %b\ %e\ %I:%M\ %p')}\ %5l,%-6(%c%V%)%)%y

" color schemes
set t_Co=256

" Switch syntax highlighting on and highlight the last search pattern
syntax on
set hlsearch

" Spell checking
if has("spell")
    set spell
endif

" tab completion for file names
set wildmode=longest,list,full
set wildmenu

" Add other file types that aren't automatically detected
augroup filetypedetect
 au BufNewFile,BufRead *w3mtmp* setf web
 au BufNewFile,BufRead *tmp/mutt* setf mail
 au BufNewFile,BufRead *.txt    setf text
 au BufNewFile,BufRead *.asmx   setf cs
 au BufNewFile,BufRead *.rpy    setf python
 au BufNewFile,BufRead *.kid    setf kid
 au BufNewFile,BufRead *.mako set ft=mako
 autocmd BufNewFile,BufRead *.yaml set filetype=yaml
augroup END

" Settings for latex and html editing
autocmd Filetype tex,bib,html,xhtml,xml,xslt,xsd,kid,javascript set sw=2  " two space indenting
autocmd Filetype tex,bib,html,xhtml,xml,xslt,xsd,kid,javascript set expandtab " spaces instead of tabs 

" Settings for python editing
" -- check out http://www.vex.net/~x/python_and_vim.html
" Use 4 spaces for new tabs, but still keep the 8 space default
" for display. These settings overall give us nice python code
" with normal display of tabs otherwise.
autocmd Filetype python set shiftwidth=4 
autocmd Filetype python set expandtab "Use spaces instead of tab characters
let python_highlight_all = 1

" Settings for haskell
autocmd Filetype haskell set shiftwidth=4 
autocmd Filetype haskell set expandtab "Use spaces instead of tab characters
autocmd Filetype haskell set tw=80 " line wrapping 

" Settings for perl editing
autocmd Filetype perl set shiftwidth=2
autocmd Filetype perl set expandtab

" Settings for java editing
autocmd Filetype java set shiftwidth=4
autocmd Filetype java set expandtab
"autocmd Filetype java set tw=80

" Settings for c sharp editing
autocmd Filetype cs set shiftwidth=2
autocmd Filetype cs set expandtab
"autocmd Filetype cs set tw=80

" Settings for idl editing
autocmd Filetype idl set shiftwidth=2
autocmd Filetype idl set expandtab
"autocmd Filetype idl set tw=80
autocmd Filetype idl set cindent

" Settings for SQL editing
autocmd Filetype sql set sw=2
autocmd Filetype sql set expandtab
"autocmd Filetype sql set tw=80

" Other filetypes where we want spaces instead of tabs
autocmd Filetype tex,bib,text,cov,mail,html,rst set expandtab
autocmd Filetype rst set shiftwidth=4
autocmd Filetype markdown set expandtab
autocmd Filetype markdown set shiftwidth=2
autocmd Filetype markdown set tw=80
autocmd Filetype yaml set expandtab
autocmd Filetype yaml set shiftwidth=2
autocmd Filetype mako set expandtab
autocmd Filetype mako set shiftwidth=2

" Settings for clojure editing
autocmd Filetype clojure set shiftwidth=2 
autocmd Filetype clojure set expandtab 

" Setting for R
let vimrplugin_underscore=0

" Keyboard mappings for cutting and pasting large amounts of text without
" changing it (ie. inserting tabs). 
" See http://www.vim.org/faq/  in 5.17 for more explanation
nmap    _Y      :.w! ~/.vi_tmp<CR>
vmap    _Y      :w! ~/.vi_tmp<CR>
nmap    _P      :r ~/.vi_tmp<CR>

" Key mappings for emacs keystrokes for brain freezes
map <C-s> :w<CR>
imap <C-s> :w<CR>
map <C-c> :q<CR>
imap <C-c> :q<CR>

" Make the backspace always delete, instead of making a ^?.
" This makes using vim much easier with *BSDs, which don't like to
" automagically map backspace to delete
cnoremap <DEL> <BACKSPACE> 
inoremap <DEL> <BACKSPACE>

let base16colorspace=256
set background=dark
colorscheme base16-tomorrow
" vi:ts=4 sw=4 expandtab

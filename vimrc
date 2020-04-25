" Vim configuration file
"
" Pathogen package management
" Clone packages to use under ~/.vim/bundle
execute pathogen#infect()

" general settings
set nobackup	" do not keep a backup file, use versions instead
set viminfo='50,\"50 " read/write a .viminfo file
set showcmd    "show incomplete commands
set showmatch  "show matching parentheses and brackets and things
set showmode   "show the current mode (insert, etc)
set vb         "shut off beeping
" set clipboard=unnamedplus  "use system clipboard
" " Prevent x from overriding what's in the clipboard.
" noremap x "_x
" noremap X "_x

silent !mkdir -p ~/.vimswap
set directory=~/.vimswap//

set rulerformat=%55(%{strftime('%a\ %b\ %e\ %I:%M\ %p')}\ %5l,%-6(%c%V%)%)%y

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

" Interact with IPython
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
let g:slime_default_config = {'socket_name': get(split($TMUX, ','), 0), 'target_pane': '{right-of}' }
let g:ipython_cell_delimit_cells_by = 'tags'
autocmd FileType python nnoremap <buffer> <Leader>r :IPythonCellRun<CR>
autocmd FileType python nnoremap <buffer> <Leader>e :IPythonCellExecuteCell<CR>

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
autocmd Filetype clojure let maplocalleader=','
autocmd FileType clojure nnoremap <buffer> <localleader>e :Eval<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>E :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>r :Require<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>R :Require!<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>t :.RunTests<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>T :RunAllTests<cr>

" Table formatting with vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Setting for R
let vimrplugin_underscore=0

" Keyboard mappings for cutting and pasting large amounts of text without
" changing it (ie. inserting tabs).
" See http://www.vim.org/faq/  in 5.17 for more explanation
nmap    _O      :.w! ~/.vi_tmp<CR>
vmap    _O      :w! ~/.vi_tmp<CR>
nmap    _P      :r ~/.vi_tmp<CR>

" Key mappings for emacs keystrokes for brain freezes
map <C-s> :w<CR>
imap <C-s> :w<CR>
map <C-c> :q<CR>
imap <C-c> :q<CR>

" Ale code cleanup and linting
let g:ale_linters = {}
let g:ale_linters['python'] = ['flake8']
let g:ale_fixers = {}
let g:ale_fixers['python'] =  ['yapf']

" project search with fzf
set rtp+=~/.fzf

" Make the backspace always delete, instead of making a ^?.
" This makes using vim much easier with *BSDs, which don't like to
" automagically map backspace to delete
cnoremap <DEL> <BACKSPACE>
inoremap <DEL> <BACKSPACE>

" color schemes
set background=dark
colorscheme gruvbox
" disable Background Color Erase (BCE) so that color schemes
" render properly when inside 256-color tmux and GNU screen.
" see also http://snk.tuxfamily.org/log/vim-256color-bce.html
if &term =~ '256color'
	set t_Co=256
	set t_ut=
endif
" vi:ts=4 sw=4 expandtab

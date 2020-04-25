#!/usr/bin/env bb

(def plugins
  {"base16-vim" "https://github.com/chriskempson/base16-vim.git"
   "fzf.vim" "https://github.com/junegunn/fzf.vim.git"
   "gruvbox" "https://github.com/morhetz/gruvbox.git"
   "supertab" "https://github.com/ervandew/supertab.git"
   "vim-better-whitespace" "https://github.com/ntpeters/vim-better-whitespace.git"
   "vim-clojure-static" "https://github.com/guns/vim-clojure-static.git"
   "vim-easy-align" "https://github.com/junegunn/vim-easy-align"
   "vim-fireplace" "git://github.com/tpope/vim-fireplace.git"
   "vim-gnupg" "https://github.com/jamessan/vim-gnupg.git"
   "vim-ipython-cell" "https://github.com/hanschen/vim-ipython-cell.git"
   "vim-pathogen" "https://github.com/tpope/vim-pathogen.git"
   "vim-python-pep8-indent" "https://github.com/Vimjas/vim-python-pep8-indent.git"
   "vim-repeat" "git://github.com/tpope/vim-repeat.git"
   "vim-sensible" "https://github.com/tpope/vim-sensible.git"
   "vim-sexp" "https://github.com/guns/vim-sexp.git"
   "vim-sexp-mappings-for-regular-people" "https://github.com/tpope/vim-sexp-mappings-for-regular-people.git"
   "vim-slime" "git://github.com/jpalardy/vim-slime.git"
   "vim-surround" "https://github.com/tpope/vim-surround.git"})

(def bundle-dir "~/.vim/bundle")
(when-not (.exists (io/file bundle-dir))
  (shell/sh "mkdir" "-p" bundle-dir))

(doseq [plugin giturl]
  (let [out-dir (io/file bundle-dir plugin)]
    (when-not (.exists out-dir)
      (shell/sh "git" "clone" giturl (str out-dir)))))

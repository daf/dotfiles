" vim: foldmethod=marker

" Plugins {{{
call plug#begin("~/.local/share/nvim/plugged")

" Colors
Plug 'noahfrederick/Hemisu'
Plug 'hallski/spacedust-theme', { 'rtp': 'Vim' }
Plug 'altercation/vim-colors-solarized'
Plug 'sjl/badwolf'
Plug 'larssmit/vim-getafe'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'daf/amy-vim-colorscheme'
Plug 'chriskempson/base16-vim'
Plug 'MidnaPeach/neonwave.vim'
Plug 'gregsexton/Muon'
Plug 'whatyouhide/vim-gotham'
Plug 'hachy/eva01.vim'
Plug 'zsoltf/vim-maui'
Plug 'he11yeah/VIvid.vim'
Plug 'morhetz/gruvbox'
Plug 'atelierbram/vim-colors_duotones'
Plug 'mhartington/oceanic-next'
Plug 'vim-scripts/sift'
Plug 'tstelzer/welpe.vim'
Plug 'ipsod/nes.vim'
Plug 'zandrmartin/vim-distill'
Plug 'dracula/vim', { 'name': 'dracula' }
Plug 'baskerville/bubblegum'
Plug 'daylerees/colour-schemes', { 'rtp': 'vim' }
Plug 'rainglow/vim', { 'name': 'rainglow' }
Plug 'MvanDiemen/ghostbuster'
Plug 'encody/vim-colors', { 'name': 'lyla-colors' }
Plug 'owickstrom/vim-colors-paramount'
Plug 'arcticicestudio/nord-vim'
Plug 'chmllr/elrodeo-vim-colorscheme'
Plug 'shattered/vimcolors', { 'name': 'more-colors' }
Plug 'iceisspetrel/Monrovia'
Plug 'AlessandroYorba/Despacio'
Plug 'AlessandroYorba/Sierra'
Plug 'AlessandroYorba/Alduin'
Plug 'w0ng/vim-hybrid'
Plug 'rakr/vim-one'
Plug 'lu-ren/SerialExperimentsLain'
Plug 'preocanin/greenwint'
Plug 'bluz71/vim-moonfly-colors'
Plug 'ajmwagar/vim-dues'
Plug 'junegunn/seoul256.vim'
Plug 'sts10/vim-mustard'
Plug 'archSeer/colibri.vim'
Plug 'trevordmiller/nova-vim'
Plug 'fenetikm/falcon'
Plug 'NerdyPepper/agila.vim'

" File support
Plug 'ekalinin/Dockerfile.vim'

" Behavior
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-obsession'

" IDEish
Plug 'mileszs/ack.vim'
Plug 'thisivan/vim-bufexplorer'
Plug 'scrooloose/nerdtree'
Plug 'kien/ctrlp.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'mhinz/vim-signify'
Plug 'bling/vim-airline'
Plug 'prettier/vim-prettier', { 'do': 'yarn install', 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue'] }
Plug 'severin-lemaignan/vim-minimap'

call plug#end()
" }}}

set hlsearch    " search with highlight
set incsearch   " incremental search
set number      " line numbers
set wildmenu    " nicer menu
set ruler       " show cursor pos in status line
set cul         " highlight current line
set scrolloff=5 " scroll offset
set hidden      " allows undo buffers to remain after switching back/forth
set splitbelow  " direction of split opens
set splitright  " "

" per project settings
set exrc
set secure

" space/tabs
set expandtab
set shiftwidth=4
set tabstop=4

set termguicolors
colors distill

" mouse/terminal
if !has('gui_running')
    set mouse=a
    set mousemodel=popup
    set termencoding=utf-8
endif

" binds
map <F9> :NERDTreeToggle
let g:NERDTreeQuitOnOpen = 1

" line length for git commit messages
autocmd FileType gitcommit set colorcolumn=73
autocmd FileType gitcommit set textwidth=72

" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd Syntax * syn match ExtraWhitespace /\s\+$\| \+\ze\t/

" ctrl+p options
let g:ctrlp_map='<Leader>p'
let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files', 'find %s -type f']


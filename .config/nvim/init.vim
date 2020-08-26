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
Plug 'AlessandroYorba/Breve'
Plug 'AlessandroYorba/Arcadia'
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
Plug 'KKPMW/sacredforest-vim'
Plug 'tjammer/blayu.vim'
Plug 'rakr/vim-two-firewatch'
Plug 'atelierbram/Base2Tone-vim'
Plug 'metalelf0/base16-black-metal-scheme'
Plug 'agreco/vim-citylights'
Plug 'caksoylar/vim-mysticaltutor'
Plug 'Nequo/vim-allomancer'
Plug 'qqwaszxxx/vim-battlestation'
Plug 'oka-h/yozakura.vim'
Plug 'tyrannicaltoucan/vim-quantum'
Plug 'jonathanfilip/vim-lucius'
Plug 'jpo/vim-railscasts-theme'
Plug 'smallwat3r/vim-hashpunk-sw'
Plug 'beikome/cosme.vim'
Plug 'DankNeon/vim', { 'name': 'dank-neon' }
Plug 'nightsense/cosmic_latte'
Plug 'cseelus/vim-colors-tone'
Plug 'haishanh/night-owl.vim'
Plug 'ntk148v/vim-horizon'
Plug 'chase/focuspoint-vim'
Plug 'itchyny/landscape.vim'
Plug 'jaredgorski/SpaceCamp'
Plug 'Rigellute/shades-of-purple.vim'
Plug 'smallwat3r/vim-mono-sw'
Plug 'NLKNguyen/papercolor-theme'
Plug 'NerdyPepper/vim-colors-plain'
Plug 'balanceiskey/vim-framer-syntax'
Plug 'vim-scripts/oceandeep'
Plug 'srcery-colors/srcery-vim'
Plug 'Rigellute/rigel'
Plug 'hauleth/blame.vim'
Plug 'tpozzi/Sidonia'
Plug 'vim-scripts/TaQua'
Plug 'matveyt/vim-modest'
Plug 'gertjanreynaert/cobalt2-vim-theme'
Plug 'maslaral/the-creator.vim'
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'flrnprz/candid.vim'
Plug 'arzg/vim-corvine'
Plug 'skbolton/embark'

" File support
"Plug 'ekalinin/Dockerfile.vim'
"Plug 'posva/vim-vue'
"Plug 'digitaltoad/vim-pug'
"Plug 'pangloss/vim-javascript'
Plug 'sheerun/vim-polyglot'
Plug 'peitalin/vim-jsx-typescript'
Plug 'leafgarland/typescript-vim'

" Behavior
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-obsession'
Plug 'embear/vim-localvimrc'
Plug 'michaeljsmith/vim-indent-object'
Plug 'mattn/emmet-vim'

" IDEish
Plug 'mileszs/ack.vim'
Plug 'thisivan/vim-bufexplorer'
Plug 'kien/ctrlp.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'mhinz/vim-signify'
Plug 'bling/vim-airline'
Plug 'chase/vim-airline-focuspoint'
Plug 'prettier/vim-prettier', { 'do': 'yarn install', 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue'] }
Plug 'severin-lemaignan/vim-minimap'
Plug 'janko-m/vim-test'
Plug 'benmills/vimux'
Plug 't9md/vim-quickhl'
Plug 'RRethy/vim-illuminate'
Plug 'biskark/vim-ultimate-colorscheme-utility'
Plug 'tpope/vim-vinegar'
Plug 'w0rp/ale'
Plug 'junegunn/goyo.vim'
Plug 'thiagoalessio/rainbow_levels.vim'

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

set expandtab
set shiftwidth=2
set tabstop=2
set listchars=tab:→\ ,extends:»,precedes:«,nbsp:·,trail:·
set showbreak=↪\

set undofile
set undodir=~/.local/share/nvim/undo

set termguicolors
colors badwolf

" SECURITY
" https://github.com/numirias/security/blob/master/doc/2019-06-04_ace-vim-neovim.md
set modelines=0
set nomodeline

" mouse/terminal
if !has('gui_running')
    set mouse=a
    set mousemodel=popup
    set termencoding=utf-8
endif

" splits
set fillchars=vert:┃ " for vsplits
set fillchars+=fold:· " for folds

" binds
noremap <Leader>bd :Bclose<CR>  " close buffer (butane)
nmap <silent> <Leader>t :TestNearest<CR>
nmap <silent> <Leader>T :TestFile<CR>
nmap <silent> <Leader>a :TestSuite<CR>
nmap <silent> <Leader>l :TestLast<CR>
nmap <silent> <Leader>g :TestVisit<CR>
nmap <Leader>m <Plug>(quickhl-manual-this)
vmap <Leader>m <Plug>(quickhl-manual-this)
nmap <Leader>M <Plug>(quickhl-manual-reset)
vmap <Leader>M <Plug>(quickhl-manual-reset)

" line length for git commit messages
autocmd FileType gitcommit set colorcolumn=73
autocmd FileType gitcommit set textwidth=72

" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd Syntax * syn match ExtraWhitespace /\s\+$\| \+\ze\t/

" ctrl+p options
let g:ctrlp_map='<Leader>p'
let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files', 'find %s -type f']

" test options
let test#strategy='vimux'
let test#python#runner = 'pytest'

" python
let g:python3_host_prog='/home/daf/miniconda3/envs/nvim/bin/python'
let g:python_host_prog='/home/daf/miniconda3/envs/nvim/bin/python'

" localvimrc options
let g:localvimrc_persistent=2
let g:localvimrc_persistence_file='$HOME/.local/share/nvim/.localvimrcpersistence'
let g:localvimrc_whitelist='/home/daf/dev/.*'

" ultimate colorscheme options (custom, don't change on enter)
let g:ulti_color_always_random=3

" ale
let b:ale_linters = {'python': ['flake8']}
let g:ale_python_flake8_options = "--ignore E265,E501,E124,E203,E221"
let b:ale_fixers = {'python': ['black']}


" https://www.lucasfcosta.com/2019/02/10/terminal-guide-2019.html
" fix files on save
" let g:ale_fix_on_save = 1
"
" lint after 1000ms after changes are made both on insert mode and normal mode
let g:ale_lint_on_text_changed = 'always'
let g:ale_lint_delay = 1000

" use nice symbols for errors and warnings
let g:ale_sign_warning = '⚠'
let g:ale_sign_error = "◉"

" " fixer configurations
" let g:ale_fixers = {
" \   '*': ['remove_trailing_lines', 'trim_whitespace'],
" \}

" make emmet behave well with JSX in JS and TS files
let g:user_emmet_settings = {
\  'javascript' : {
\      'extends' : 'jsx',
\  },
\  'typescript' : {
\      'extends' : 'tsx',
\  },
\}

" polyglot options
let g:polyglot_disabled = ['org']

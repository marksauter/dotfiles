" Neovim config
" https://github.com/marksauter/dotfiles
" --------------------------------------

if &compatible
  set nocompatible
endif

" Python
" ------
let g:python3_host_prog='/usr/bin/python3'

" Vim Initialization
" ------------------
" Set custom augroup
augroup user_events
  autocmd!
augroup END

" Initialize startup settings
if has('vim_starting')
  " Use spacebar as leader and ; as secondary-leader
  " Required before loading plugins!
  let g:mapleader = "\<Space>"
  let g:maplocalleader=';'

  " Release keymappings prefixes, evict entirely for use of plug-ins.
  nnoremap <Space>  <Nop>
  xnoremap <Space>  <Nop>
  nnoremap ,        <Nop>
  xnoremap ,        <Nop>
  nnoremap ;        <Nop>
  xnoremap ;        <Nop>

  if ! has('nvim') && ! has('gui_running') && ! has('win32') && ! has('win64')
    " Enable 256 color terminal
    set t_Co=256

    " Paste
    " Credits: https://github.com/Shougo/shougo-s-github
    " ---
    let &t_ti .= "\e[?2004h"
    let &t_te .= "\e[?2004l"
    let &pastetoggle = "\e[201~"

    function! s:XTermPasteBegin(ret) abort
      setlocal paste
      return a:ret
    endfunction

    noremap  <special> <expr> <Esc>[200~ <SID>XTermPasteBegin('0i')
    inoremap <special> <expr> <Esc>[200~ <SID>XTermPasteBegin('')
    cnoremap <special> <Esc>[200~ <nop>
    cnoremap <special> <Esc>[201~ <nop>

    " Mouse settings
    " ---
    if has('mouse')
      if has('mouse_sgr')
        set ttymouse=sgr
      else
        set ttymouse=xterm2
      endif
    endif

    " Cursor-shape
    " Credits: https://github.com/wincent/terminus
    " ---
    " Detect terminal
    let s:tmux = exists('$TMUX')

    " 1 or 0 -> blinking block
    " 2 -> solid block
    " 3 -> blinking underscore
    " 4 -> solid underscore
    " Recent versions of xterm (282 or above) also support
    " 5 -> blinking vertical bar
    " 6 -> solid vertical bar
    let s:normal_shape = 0
    let s:insert_shape = 5
    let s:replace_shape = 3

    let s:cursor_shape_to_vte_shape = {1: 6, 2: 4, 0: 2, 5: 6, 3: 4}
    let s:insert_shape = s:cursor_shape_to_vte_shape[s:insert_shape]
    let s:replace_shape = s:cursor_shape_to_vte_shape[s:replace_shape]
    let s:normal_shape = s:cursor_shape_to_vte_shape[s:normal_shape]
    let s:start_insert = "\<Esc>[" . s:insert_shape . ' q'
    let s:start_replace = "\<Esc>[" . s:replace_shape . ' q'
    let s:end_insert = "\<Esc>[" . s:normal_shape . ' q'


    function! s:tmux_wrap(string)
      if strlen(a:string) == 0 | return '' | end
      let l:tmux_begin = "\<Esc>Ptmux;"
      let l:tmux_end = "\<Esc>\\"
      let l:parsed = substitute(a:string, "\<Esc>", "\<Esc>\<Esc>", 'g')
      return l:tmux_begin.l:parsed.l:tmux_end
    endfunction

    if s:tmux
      let s:start_insert = s:tmux_wrap(s:start_insert)
      let s:start_replace = s:tmux_wrap(s:start_replace)
      let s:end_insert = s:tmux_wrap(s:end_insert)
    endif
    
    let &t_SI = s:start_insert
    if v:version > 704 || v:version == 704 && has('patch687')
      let &t_SR = s:start_replace
    end
    let &t_EI = s:end_insert
    
    " Tmux specific settings
    " ---
    if s:tmux
      set ttyfast
    
      " Set Vim-specific sequences for RGB colors
      " Fixes 'termguicolors' usage in tmux
      " :h xterm-true-color
      let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
      let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    
      " Assigns some xterm(1)-style keys to escape sequences passed by tmux
      " when 'xterm-keys' is set to 'on'.  Inspired by an example given by
      " Chris Johnsen at https://stackoverflow.com/a/15471820
      " Credits: Mark Oteiza
      " Documentation: help:xterm-modifier-keys man:tmux(1)
      execute "set <xUp>=\e[1;*A"
      execute "set <xDown>=\e[1;*B"
      execute "set <xRight>=\e[1;*C"
      execute "set <xLeft>=\e[1;*D"
    
      execute "set <xHome>=\e[1;*H"
      execute "set <xEnd>=\e[1;*F"
    
      execute "set <Insert>=\e[2;*~"
      execute "set <Delete>=\e[3;*~"
      execute "set <PageUp>=\e[5;*~"
      execute "set <PageDown>=\e[6;*~"
    
      execute "set <xF1>=\e[1;*P"
      execute "set <xF2>=\e[1;*Q"
      execute "set <xF3>=\e[1;*R"
      execute "set <xF4>=\e[1;*S"
    
      execute "set <F5>=\e[15;*~"
      execute "set <F6>=\e[17;*~"
      execute "set <F7>=\e[18;*~"
      execute "set <F8>=\e[19;*~"
      execute "set <F9>=\e[20;*~"
      execute "set <F10>=\e[21;*~"
      execute "set <F11>=\e[23;*~"
      execute "set <F12>=\e[24;*~"
    endif
  endif
endif

" Install vim-plug if not installed
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

" Plugins
" =======
call plug#begin()

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-endwise'
Plug 'rstacruz/vim-closer'
Plug 'christoomey/vim-tmux-navigator'

" Completion
" ----------
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

Plug 'ncm2/ncm2-bufword' " completion for words from current buffer
Plug 'ncm2/ncm2-path' " enables completion of file paths
Plug 'ncm2/ncm2-racer' " completion for rust-lang
Plug 'wellle/tmux-complete.vim' " completion between tmux sessions

" NOTE: replace this with neovim/nvim-lsp when nvim is updated
Plug 'autozimu/LanguageClient-neovim', {
  \ 'branch': 'next',
  \ 'do': 'bash install.sh',
  \ }

Plug 'dense-analysis/ale'

" Interface
" ---------
Plug 'itchyny/lightline.vim'
Plug 'mgee/lightline-bufferline' " For tabs on top
Plug 'ryanoasis/vim-devicons' " For filetype icons

Plug 'tpope/vim-fugitive'

Plug 'chriskempson/base16-vim'

Plug 'rbgrouleff/bclose.vim'
Plug 'junegunn/fzf.vim'

" Languages
" ---------
Plug 'rust-lang/rust.vim'
Plug 'HerringtonDarkholme/yats.vim'

" Commands
" --------
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Motion
" ------
Plug 'easymotion/vim-easymotion'

call plug#end()

" ALE
" ---
let g:ale_rust_rls_executable = 'ra_lsp_server'
let g:ale_rust_rls_toolchain = ''

" NCM2
" ----
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" set completeopt to be what ncm2 expects
set completeopt=noinsert,menuone,noselect

" LSP
" ---
" let g:LanguageClient_trace = 'verbose'
" let g:LanguageClient_loggingFile = '~/.vim/LanguageClient.log'
" let g:LanguageClient_loggingLevel = 'DEBUG'
let g:LanguageClient_serverCommands = {
  \ 'rust': ['rust-analyzer'],
  \ 'typescript': ['typescript-language-server', '--stdio'],
  \ }

" Format
" ---------
let g:rustfmt_autosave = 1


" Lightline
" ---------
let g:lightline = {
  \ 'separator': { 'left': '', 'right': '' },
  \ 'subseparator': { 'left': '', 'right': '' },
  \ 'tabline': {
  \   'left': [['buffers']],
  \   'right': [[ 'exit' ]],
  \ },
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'readonly', 'filename', 'modified'] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'fugitive#head'
  \ },
  \ 'component_expand': {
  \   'buffers': 'lightline#bufferline#buffers',
	\		'lsp_status': 'LightlineLSPStatus',
  \ },
  \ 'component_type': {
  \   'buffers': 'tabsel'
  \ },
  \ }
let g:lightline#bufferline#shorten_path = 1
let g:lightline#bufferline#enable_devicons = 1

augroup LanguageClient_config
	au!
	au User LanguageClientStarted call LSPUpdateStatus(1)
	au User LanguageClientStopped call LSPUpdateStatus(0)
augroup END
let g:lsp_status = 0
function! LSPUpdateStatus(status) abort
	let g:lsp_status = a:status
	call lightline#update()
endfunction
function! LightlineLSPStatus() abort
	return g:lsp_state == 1 ? 'LSP' : ''
endfunction

" FZF
" ---
nnoremap <silent><localleader>f :Files<CR>
nnoremap <silent><localleader>b :Buffers<CR>
nnoremap <silent><localleader>g :Rg<CR>
if has ('nvim')
  augroup fzf_setup
    autocmd!
    autocmd TermOpen term://*FZF tnoremap <silent> <buffer><nowait> <esc> <c-c>
  augroup END
end

function! Fzf_dev()
  function! s:files()
    let files = split(system($FZF_DEFAULT_COMMAND), '\n')
    return s:prepend_icon(files)
  endfunction

  function! s:prepend_icon(candidates)
    let result = []
    for candidate in a:candidates
      let filename = fnamemodify(candidate, ':p:t')
      let icon = WebDevIconsGetFileTypeSymbol(filename, isdirectory(filename))
      call add(result, printf("%s %s", icon, candidate))
    endfor

    return result
  endfunction

  function! s:edit_file(item)
    let parts = split(a:item, ' ')
    let file_path = get(parts, 1, '')
    execute 'silent e' file_path
  endfunction

  call fzf#run({
    \ 'source': <sid>files(),
    \ 'sink':   function('s:edit_file'),
    \ 'options': '-m -x +s',
    \ 'down':    '40%' })
endfunction


command! FilesWithIcon :call Fzf_dev()

" File Explorer
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25
nnoremap <localleader>a :Lexplore<CR>

" General
" =======
set mouse=nv                 " Disable mouse in command-line mode
set modeline                 " automatically setting options from modelines
set report=0                 " Don't report on line changes
set errorbells               " Trigger bell on error
set visualbell               " Use visual bell instead of beeping
set signcolumn=yes           " Always show signs column
set hidden                   " hide buffers when abandoned instead of unload
set fileformats=unix,dos,mac " Use Unix as the standard file type
set magic                    " For regular expressions turn magic on
set path=.,**                " Directories to search when using gf
set virtualedit=block        " Position cursor anywhere in visual block
set synmaxcol=1000           " Don't syntax highlight long lines
set formatoptions+=1         " Don't break lines after a one-letter word
set formatoptions-=t         " Don't auto-wrap text

" Enable 24-bit RGB color in the TUI
if has('termguicolors')
        set termguicolors
endif

if has('clipboard')
        set clipboard& clipboard+=unnamedplus
endif

" Wildmenu
if has('wildmenu')
  set nowildmenu
  set wildmode=list:longest,full
  set wildoptions=tagfile
  set wildignorecase
  set wildignore+=.git,.hg,.svn,.stversions,*.pyc,*.spl,*.o,*.out,*~,%*
  set wildignore+=*.jpg,*.jpeg,*.png,*.gif,*.zip,**/tmp/**,*.DS_Store
  set wildignore+=**/node_modules/**,**/bower_modules/**,*/.sass-cache/*
  set wildignore+=application/vendor/**,**/vendor/ckeditor/**,media/vendor/**
  set wildignore+=__pycache__,*.egg-info,.pytest_cache
endif

" Vim Directories
" ---------------
set undofile swapfile nobackup

" History saving
if has('nvim')
  set shada='300,<50,@100,s10,h
else
  set viminfo='300,<10,@50,h,n$DATA_PATH/viminfo
endif

" Secure sensitive information, disable backup files in temp directories
if exists('&backupskip')
  set backupskip+=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*
  set backupskip+=.vault.vim
endif

" Disable swap/undo/viminfo/shada files in temp directories or shm
augroup user_secure
  autocmd!
  silent! autocmd BufNewFile,BufReadPre
    \ /tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*,.vault.vim
    \ setlocal noswapfile noundofile nobackup nowritebackup viminfo= shada=
augroup END

" Tabs and Indents
" ----------------
set textwidth=80    " Text width maximum chars before wrapping
set noexpandtab     " Don't expand tabs to spaces.
set tabstop=2       " The number of spaces a tab is
set softtabstop=2   " While performing editing operations
set shiftwidth=2    " Number of spaces to use in auto(indent)
set smartindent     " Smart autoindenting on new lines
set shiftround      " Round indent to multiple of 'shiftwidth'

" Timing
" ------
set timeout ttimeout
set timeoutlen=750  " Time out on mappings
set updatetime=400  " Idle time to write swap and trigger CursorHold
set ttimeoutlen=10  " Time out on key codes

" Searching
" ---------
set ignorecase      " Search ignoring case
set smartcase       " Keep case when searching with *
set infercase       " Adjust case in insert completion mode
set wrapscan        " Searches wrap around the end of the file
set showmatch       " Jump to matching bracket
set matchpairs+=<:> " Add HTML brackets to pair matching
set matchtime=1     " Tenths of a second to show the matching paren
set cpoptions-=m    " showmatch will wait 0.5s or until a char is typed
set showfulltag     " Show tag and tidy search in completion

if exists('+inccommand')
  set inccommand=nosplit
endif


if executable('rg')
  set grepformat=%f:%l:%m
  let &grepprg = 'rg --vimgrep' . (&smartcase ? ' --smart-case' : '')
endif

" Behavior
" --------
set nowrap                      " No wrap by default
set linebreak                   " Break long lines at 'breakat'
set breakat=\ \ ;:,!?           " Long lines break chars
set nostartofline               " Cursor in same column for few commands
set whichwrap+=h,l,<,>,[,],~    " Move to following line on certain keys
set splitbelow splitright       " Splits open bottom right
set switchbuf=useopen,usetab    " Jump to the first open window in any tab
set switchbuf+=vsplit           " Switch buffer behavior to vsplit
set diffopt=filler,iwhite       " Diff mode: show fillers, ignore whitespace
set completeopt=menuone         " Always show menu, even for one item
set completeopt+=noselect       " Do not select a match in the menu

if has('patch-7.4.775')
  " Do not insert any text for a match until the user selects from menu
  set completeopt+=noinsert
endif

if has('patch-8.1.0360') || has('nvim-0.4')
  set diffopt+=internal,algorithm:patience
  " set diffopt=indent-heuristic,algorithm:patience
endif

" Editor UI
" ---------
set noshowmode          " Don't show mode in cmd window
set shortmess=aoOTI     " Shorten messages and don't show intro
" set nonumber            " Don't show line numbers
set number              " Show line numbers
" set list                " Show hidden characters

set showtabline=2       " Always show the tabs line
set winwidth=30         " Minimum width for active window
set winminwidth=10      " Minimum width for inactive windows
set winheight=4         " Minimum height for active window
set winminheight=1      " Minimum height for inactive window
set pumheight=15        " Pop-up menu's line height
set helpheight=12       " Minimum help window height
set previewheight=12    " Completion preview height

set showcmd             " Show command in status line
set cmdheight=2         " Height of the command line
set cmdwinheight=5      " Command-line lines
set equalalways         " Resize windows on split or close
set colorcolumn=80      " Highlight the 80th character limit

if has('folding')
  set foldenable
  set foldmethod=syntax
  set foldlevelstart=99
endif

" UI Symbols
set showbreak=↪

if has('patch-7.4.314')
  " Do not display completion messages
  set shortmess+=c
endif

if has('patch-7.4.1570')
  " Do not display message when editing files
  set shortmess+=F
endif

if has('conceal') && v:version >= 703
  " For snippet_complete marker
  set conceallevel=2 concealcursor=niv
endif

if exists('&pumblend')
  " pseudo-transparency for completion menu
  "set pumblend=20
endif

if exists('&winblend')
  " pseudo-transparency for floating window
  "set winblend=20
endif

" File Types
" ==========

augroup user_plugin_filetype
  autocmd!

  " Reload vim config automatically
  autocmd BufWritePost init.vim,.vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc
    \ so $MYVIMRC
    \ | if has('gui_running')
    \ | so $MYGVIMRC
    \ | endif

  " Force write shada on leaving nvim
  autocmd VimLeave * if has('nvim') | wshada! | else | wviminfo! | endif

  " Update filetype on save if empty
  autocmd BufWritePost * nested
    \ if &l:filetype ==# '' || exists('b:ftdetect')
    \ |   unlet! b:ftdetect
    \ |   filetype detect
    \ | endif

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  autocmd BufReadPost *
    \ if &ft !~# 'commit' && ! &diff &&
    \      line("'\"") >= 1 && line("'\"") <= line("$")
    \|   execute 'normal! g`"zvzz'
    \| endif
augroup END

" Key-mappings
" ============

" Clear highlight on pressing Esc
nnoremap <silent><Esc> :nohlsearch<CR><Esc>

" Use <Tab> to select the popup menu
inoremap <expr> <Tab> pumvisible() ? "<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "<C-p>" : "\<S-Tab>"

" Retrace movements
nnoremap B <C-o>
nnoremap W <C-i>

" Toggle fold
nnoremap <CR> za

" Focus the current fold by closing all others
nnoremap <S-Return> zMzvzt

" Use backspace key for matchit.vim
nmap <BS> %
xmap <BS> %

" Start an external command with a single bang
nnoremap ! :!

" Select blocks after indenting
xnoremap < <gv
xnoremap > >gv|

" Use tab for indenting
xnoremap <Tab> >gv|
xnoremap <S-Tab> <gv
nmap >> >>_
nmap << <<_

" Navigation in command line
cnoremap <C-h> <Home>
cnoremap <C-l> <End>
cnoremap <C-j> <Left>
cnoremap <C-k> <Right>
cnoremap <C-d> <C-w>

" :quit man/help pages with 'q'
autocmd user_events FileType man,help nnoremap <silent><buffer> q :<C-u>:quit<CR>

" LSP
nmap <silent> <F5> :call LanguageClient_contextMenu()<CR>
nmap <silent> gd :<C-u>call LanguageClient#textDocument_definition()<CR>
nmap <silent> gy :<C-u>call LanguageClient#textDocument_typeDefinition()<CR>
nmap <silent> gr :<C-u>call LanguageClient#textDocument_references()<CR>
nmap <silent> <leader>rn :<C-u>call LanguageClient#textDocument_rename()<CR>

" Use K to show documentation in preview window.
nnoremap <silent> K :<C-u>call <SID>show_documentation()<CR>

" autocmd CursorHold * silent call LanguageClient#textDocument_documentHighlight()

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call LanguageClient#textDocument_hover()
  endif
endfunction

nmap <leader>ac :<C-u>call LanguageClient#textDocument_codeAction()<CR>

" Theme
" =====

if has('gui_running')
  set background=dark
  set lines=40
  set columns=150
endif

" BASE16-COLORSCHEME
colorscheme base16-dracula

" EasyMotion
" ======
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_prompt = 'Jump to → '
" let g:EasyMotion_keys = 'fjdksweoavn'
let g:EasyMotion_use_upper = 1
let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_smartsign_us = 1

" Jump to two characters from input
nmap ss <Plug>(easymotion-s2)
" Jump to a character from input
nmap sd <Plug>(easymotion-s)
" Jump over-windows
nmap sf <Plug>(easymotion-overwin-f)
" Jump backwards in-line
map sh <Plug>(easymotion-linebackward)
" Jump forwards in-line
map sl <Plug>(easymotion-lineforward)
" Jump downwards
map sj <Plug>(easymotion-j)
" Jump upwards
map sk <Plug>(easymotion-k)
" Jump to free-search
map s/ <Plug>(easymotion-sn)
omap s/ <Plug>(easymotion-tn)
" Smart next occurrence
map sn <Plug>(easymotion-next)
" Smart previous occurrence
map sp <Plug>(easymotion-prev)

set secure

" vim:set ft=vim et sw=2:

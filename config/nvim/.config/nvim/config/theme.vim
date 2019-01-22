
" Theme
" -----

" Enable 256 color terminal
set t_Co=256

" Enable true color
if has('termguicolors')
	set termguicolors
endif

if has('gui_running')
	set background=dark
	set lines=40
	set columns=150
endif

" BASE16-COLORSCHEME
colorscheme base16-dracula

" BASE16-CUSTOMIZATION
" function! s:base16_customize() abort
"   call Base16hi("MatchParen", g:base16_gui05, g:base16_gui03, g:base16_cterm05, g:base16_cterm03, "bold,italic", "")
" endfunction
"
" augroup on_change_colorschema
"   autocmd!
"   autocmd ColorScheme * call s:base16_customize()
" augroup END

" vim: set ts=2 sw=2 tw=80 noet :

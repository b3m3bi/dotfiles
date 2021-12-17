set nocompatible			" requerido
filetype plugin on
syntax on

" configuracion necesaria de plug
" se especifica el directorio donde van los plugins
call plug#begin(stdpath('data') . '/plugged')

" AQUI VAN LOS PLUGINS 
Plug 'itchyny/lightline.vim'                                " configura la barra de estado
Plug 'neoclide/coc.nvim', {'branch':'release'}              " ayuda de autocompletado y servicios de lenguajes
"Plug 'mattn/emmet-vim'                                      " emmet
Plug 'ctrlpvim/ctrlp.vim'                                   " buscador de archivos
"Plug 'lervag/vimtex'                                        " cosas para documentos de latex
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
"Plug 'vimwiki/vimwiki'                                      " funcionalidades para hacer textos vinculados
"Plug 'nvim-lua/plenary.nvim'                                " requerido por telescope
"Plug 'nvim-telescope/telescope.nvim'                        " buscador de archivos
"Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} " libreria para hacer parsing 
"Plug 'junegunn/fzf.vim'                                     " buscador de archivos
Plug 'jpalardy/vim-slime', {'for':'python'}
Plug 'hanschen/vim-ipython-cell', { 'for': 'python' }

" inicializar el sistema de plugins
call plug#end()	    		" requerido

set shortmess+=I			" deshabilita el mensaje de inicio
set number				    " mostrar los números de línea
"set relativenumber			" muestra los valores relativos de las líneas con respecto a la acutal
set hidden                  " para poder ocultar buffers con cambios no guardados
set mouse+=a				" habilita el mouse

set tabstop=4				" longitud de TAB es igual a 4
set shiftwidth=4			" longitud de identacion igual a 4
set softtabstop=4			" se indica el numero de columnas para un TAB
set expandtab				" expande los TABs a espacios (?)

set noshowmode 				" deshabilita --INSERT--, ya que con plugin lightline no se necesita
set updatetime=300		    " aumenta la velocidad de acutalizacion (predeterminado 4000 ms)

source ~/.config/nvim/coc.config.vim

set wildignore+=*/.git/*,*/node_modules         " patrones que ignora ctrlp

" Corriege el highlighting de los bloques de math en archivos markdown 
" ref: https://stsievert.com/blog/2016/01/06/vim-jekyll-mathjax/
function! MathAndLiquid()
	" Se definen las regiones de math mode
	syn region math start=/\$\$/ end=/\$\$/
	syn match math_block '\$[^$].\{-}\$'
	syn match liquid '{%.*%}'
	syn region highlight_block start='{% highlight .*%}' end='{%.*%}'
	syn region highlight_block start='```' end='```'
	" Se resaltan las regiones	
	hi link math Statement
	hi link liquid Statement
	hi link highlight_block Function
	hi link math_block Function
endfunction

" Se llama la función cuando se abren archivos markdown
autocmd BufRead,BufNewFile,BufEnter *.md,*.markdown call MathAndLiquid()

" configuracion de Telescope
"nnoremap <leader>ff <cmd>Telescope find_files<cr>
"nnoremap <leader>fg <cmd>Telescope live_grep<cr>
"nnoremap <leader>fb <cmd>Telescope buffers<cr>
"nnoremap <leader>fh <cmd>Telescope help_tags<cr>

let g:slime_target = "tmux"

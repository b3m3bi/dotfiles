set nocompatible			" requerido
filetype off				" requerido

" configuracion necesaria de Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'		" requerido

" AQUI VAN LOS PLUGINS 
Plugin 'itchyny/lightline.vim'		" configura la barra de estado
Plugin 'neoclide/coc.nvim'          " ayuda de autocompletado y servicios de lenguajes
Plugin 'mattn/emmet-vim'            " emmet
Plugin 'scrooloose/nerdtree'        " explorador de archivos
Plugin 'ctrlpvim/ctrlp.vim'         " buscador de archivos

" Todos los plugins deben ir arriba de esta linea
call vundle#end()			" requerido
filetype plugin indent on   " requerido

set shortmess+=I			" deshabilita el mensaje de inicio
set number				    " mostrar los números de línea
set relativenumber			" muestra los valores reltaivos de las líneas con respecto a la acutal
set hidden                  " para poder ocultar buffers con cambios no guardados
set mouse+=a				" habilita el mouse

set tabstop=4				" longitud de TAB es igual a 4
set shiftwidth=4			" longitud de identacion igual a 4
set softtabstop=4			" se indica el numero de columnas para un TAB
set expandtab				" expande los TABs a espacios (?)

set noshowmode 				" deshabilita --INSERT-- ya que con lightline no se necesita
set updatetime=300          " aumenta la velocidad de acutalizacion (predeterminado 4000 ms)

source ~/.config/nvim/coc.config.vim

set wildignore+=*/.git/*,*/node_modules         " patrones que ignora ctrlp

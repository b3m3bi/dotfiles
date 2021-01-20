# Notas de configuración

## Instalaciones extra 

### Generales
- [libinput-gestures](https://github.com/bulletmark/libinput-gestures) - utilidad para agregar gestos al touchpad

### KDE 
- [virtual-desktop-bar](https://github.com/wsdfhjxc/virtual-desktop-bar) - applet para el panel

### nvim
- Instalar Vundle:
```
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```

## Hacer symlinks automaticamente con stow
- [Referencia](https://www.youtube.com/watch?v=CFzEuBGPPPg)
- Previsualizar cambios (ejecutar desde .dotfiles)
```
stow -nv *
```
- Hacer symlinks (ejecutar desde .dotfiles)
``
stow -v *
```

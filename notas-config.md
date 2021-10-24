# Notas de configuración

## Instalaciones extra 

### Generales

- [libinput-gestures](https://github.com/bulletmark/libinput-gestures) - utilidad para agregar gestos al touchpad
- Hacer que el bluetooth funcione. Ejecutar el siguiente comando y reiniciar:

```
sudo apt install pulseaudio-modue-bluetooth
```
- [Drivers de impresora hp](https://developers.hp.com/hp-linux-imaging-and-printing/) - en "testing" se necesita construir [paquetes específicos](https://tutorialforlinux.com/2021/02/25/step-by-step-python3-pyqt4-debian-bullseye-installation-guide/) - NO FUNCIONA :(

### KDE 
- [virtual-desktop-bar](https://github.com/wsdfhjxc/virtual-desktop-bar) - applet para el panel

### nvim

- Installar [vim-plug](https://github.com/junegunn/vim-plug):

```
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

### Bitwig

- Para poder instalar las dependencias de bitwig 3.+  en debian testing se necesita [habilitar la multiarquitectura](https://wiki.debian.org/Multiarch/HOWTO)

## Hacer symlinks automaticamente con stow

- [Video de referencia](https://www.youtube.com/watch?v=CFzEuBGPPPg)
- Cada carpeta (paquete) debe tener la estructura que tendía si estuviera desde el directorio `home`. E.g.: `nvim/.config/nvim/init.vim`

- Previsualizar cambios (ejecutar desde .dotfiles)

```
stow -nv nombre-paquete #nombre-de-la-carpeta
```

- Hacer symlinks (ejecutar desde .dotfiles)

```
stow -v nombre-paquete #nombre de la carpeta
```


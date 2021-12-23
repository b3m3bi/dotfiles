# Notas de configuración

## Instalaciones básicas

- Instalación mínima de KDE (`kde-plasma-desktop`) y utilidades de red (`plasma-nm`)

```
sudo apt -y install kde-plasma-desktop plasma-nm
```

- Utilidades generales de línea de comandos

```
sudo apt install vim python3-pip
```

- Utilidades generales de interfaz gráfica

```
sudo apt install firefox
```
## Instalación de libinput-gestures

- [libinput-gestures](https://github.com/bulletmark/libinput-gestures) - utilidad para agregar gestos al touchpad

```
sudo gpasswd -a $USER input # se agrega al usuario al grupo input
```

Reinciar el sistema y luego instalar y ejecutar el paquete:

```
sudo apt-get install libinput-tools wmctrl xdotool # instalar dependencias
git clone https://github.com/bulletmark/libinput-gestures.git ~/.config/libinput-gestures
cd ~/.config/libinput-gestures/
sudo make install 
libinput-gestures-setup autostart start
```

## Configuración de plasma

- [virtual-desktop-bar](https://github.com/wsdfhjxc/virtual-desktop-bar) - applet para el panel

```
git clone https://github.com/wsdfhjxc/virtual-desktop-bar.git ~/.config/virtual-desktop-bar
cd ~/.config/virtual-desktop-bar/
sudo sh ./scripts/install-dependencies-ubuntu.sh # instalar dependencias
sudo sh ./scripts/install-applet.sh
```

- [konsave](https://github.com/Prayag2/konsave) - guarda la configuración de kde

```
sudo python3 -m pip install konsave
```

- El perfil *tipo-i3* requiere: virtual-desktop-bar, libinput-gestures, firefox

```
konsave -i ~/.dotfiles/kde-config/tipo-i3.knsv # importa perfil
konsave -a tipo-i3 
```

## Otras configuraciones del sistema

- Hacer que el bluetooth funcione. Ejecutar el siguiente comando y reiniciar:

```
sudo apt install pulseaudio-modue-bluetooth
```

- [Drivers de impresora hp](https://developers.hp.com/hp-linux-imaging-and-printing/) - en "testing" se necesita construir [paquetes específicos](https://tutorialforlinux.com/2021/02/25/step-by-step-python3-pyqt4-debian-bullseye-installation-guide/) - NO FUNCIONA :(


## nvim

- Installar [vim-plug](https://github.com/junegunn/vim-plug):

```
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

## Bitwig Studio

- Para poder instalar las dependencias de bitwig 3.+  en debian testing se necesita [habilitar la multiarquitectura](https://wiki.debian.org/Multiarch/HOWTO)

```
sudo dpkg --add-architecture i368
```

## Symlinks de archivos de configuración de programas con dotfiles

- Stow es una herramienta para hacer symlinks automaticamente
- [Video de referencia](https://www.youtube.com/watch?v=CFzEuBGPPPg)
- Cada carpeta (paquete) debe tener la estructura que tendía si estuviera desde el directorio `home`. E.g.: `nvim/.config/nvim/init.vim`
- Previsualizar cambios (ejecutar desde .dotfiles)

```
stow -nv nombre-paquete <nombre-de-la-carpeta>
```

- Hacer symlinks (ejecutar desde .dotfiles)

```
stow -v nombre-paquete <nombre de la carpeta>
```


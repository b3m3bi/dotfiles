# Notas de configuración

## Instalaciones básicas

- Instalación mínima de KDE (`kde-plasma-desktop`) y utilidades de red (`plasma-nm`)

```
sudo apt -y install kde-plasma-desktop plasma-nm
```

- Utilidades generales de línea de comandos

```
sudo apt install vim python3-pip stow 
```

- Utilidades generales de interfaz gráfica

```
sudo apt install firefox ttf-mscorefonts-installer libreoffice-writer libreoffice-calc libreoffice-kf5
```

## Instalación de libinput-gestures

En wayland ya vienen por defecto los gestos, pero no parece haber forma fácil de configurarlos.

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

- [Sticky Window Snapping](https://store.kde.org/p/1112552/) - script de kwin para que las ventanas se pequen al ajustar su tamaño (si no funciona ver solución propuesta en la página).

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

## nvim

- Installar [vim-plug](https://github.com/junegunn/vim-plug):

```
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

## Bitwig Studio

- Para poder instalar las dependencias de bitwig 3.+  en debian testing se necesita [habilitar la multiarquitectura](https://wiki.debian.org/Multiarch/HOWTO)

```
sudo dpkg --add-architecture i386
sudo apt update
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

## Emacs

- Instalar la versión 28.1 desde el código fuente. [Referencia](https://www.youtube.com/watch?v=22rPCT10Kkw).

1. Instalar las dependencias necesarias (requiere que se tengan `deb-src` en el sources.list):

```
sudo apt-get build-dep emacs
```

2. Obtener src de emacs:

```
#wget https://gnu.mirror.constant.com/emacs/emacs-27.2.tar.gz
wget https://mirrors.kernel.org/gnu/emacs/emacs-28.1.tar.gz
tar -xvf emacs-28.1.tar.gz
```

3. Construir emacs:

```
cd emacs-28.1/
./configure
make
# revisar si funciona
src/emacs -Q
# instalar en sistema
sudo make install
```

### Requerimientos de algunos plugins y modos

- Requerido por **ob-jupyter**.

```
sudo pip install jupyter_console jupyter_client
```

- Requerido por 'org-latex-preview'

```
sudo apt install dvipng
```

## Latex

- Utilidades básicas (`texlive` y `texlive-latex-extra`) y `xetex` (`texlive-xetex`) que es un motor de tipografías que soporta mayores funcionalidades que `pdflatex` y `latex`.

```
sudo apt install texlive texlive-latex-extra texlive-xetex
```

- Agregar español a babel

```
sudo apt-get install texlive-lang-spanish
```

- Agregar fontawesome

```
sudo apt install texlive-fonts-extra
```

##  Webdev

### Nodejs y npm

```
sudo apt install nodejs npm
```

### Angular

- Instalar angular-cli

```
sudo npm install -g @angular/cli
```

### Mongodb

- [Instalar mongodb](https://docs.mongodb.com/manual/tutorial/install-mongodb-on-debian/)

```
curl https://www.mongodb.org/static/pgp/server-5.0.asc | gpg --dearmor | sudo dd of=/usr/local/share/keyrings/mongodb.gpg 
### revisar si hay repos más recientes a buster en enereo 2022 no hay
echo "deb [signed-by=/etc/apt/keyrings/mongodb-5.0.gpg arch=amd64] http://repo.mongodb.org/apt/debian buster/mongodb-org/5.0 main" | sudo tee /etc/apt/sources.list.d/mongodb-org-5.0.list
sudo apt update && sudo apt install mongodb-org
```

# R

- Paquetes necesarios para installar ~juicr~:

```
sudo apt install libfftw3-dev libcurl4-openssl-dev
```

Luego en R:

```
install.packages("BiocManager") 
BiocManager::install("EBImage")
install.packages("juicr")
```

# Zotero

Algunos plugins que mejoran su funcionalidad son:

- [Zotfile](http://zotfile.com/): permite adminsitrar los archivos adjuntos (e.g., renombrarlos automáticamente)
- [Better Bibtex](https://retorque.re/zotero-better-bibtex/): permite administrar los datos de bibliografía (e.g., generar automáticamente las claves de citas, exportar automáticamente el `.bib`)

# Python

### jupyter

```
pip install jupyter
```

# Servidores LSP

Para poder utilizar servidores LSP (como [eglot](https://github.com/joaotavora/eglot) de emacs) se necesitan installar los servidores:

## R

Para instalar [languageserver](https://github.com/REditorSupport/languageserver/). Primero instalar las dependencias:

```
apt install --assume-yes --no-install-recommends build-essential libcurl4-openssl-dev libssl-dev libxml2-dev r-base
```

y luego en R

```
intall.packages("languageserver")
```

## python

Para instalar [python LSP Server](https://github.com/python-lsp/python-lsp-server) se puede usar ~pip~:

```
pip install python-lsp-server
```

o se puede instalar desde ~apt~:

```
sudo apt install python3-pylsp
```

Nótese que este servidor LSP depdende de [Jedi](https://github.com/davidhalter/jedi) para el autocompletado y la ayuda. Y adicionalmente se pueden instalar dependencias opcionales para agregar otras funcionalidades como: identificar errores (pyflakes o flake8, mccabe), revisión y autoformato de estilo (pycodestyle, pydocstyle, autopep8 o YAPF), renombrar (rope). Actualmente solo uso pyflakes.

## Wallpapers

Descargar wallpapers de KDE:

```
git clone https://invent.kde.org/plasma/plasma-workspace-wallpapers
```

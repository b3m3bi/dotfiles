;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CONFIGURACIÓN GENERAL ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; se quita el mensaje de inicio
(setq inhibit-startup-message t)

;; se mandan las ediciones de customize a otro archivo y se cargan
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; se quita toolbar, menubar y scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; quitar campana en lugar de sonar hace flashes
(setq visible-bell 1)

;; seguir symlinks sin tener que confirmar
(setq vc-follow-symlinks t)

;; historial de ordenamiento de las ventanas (páneles)
;; regresar a config previa con C-{flechas}
(winner-mode 1)

;; resalta la línea en la que se está en un documento
;(global-hl-line-mode)

;; se usa package como administrador de paquetes
(require 'package)
;; se agrega el repo de melpa
;; actualizar lista de paquetes:
;; M-x package-refresh-contents RET
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; se usa use-package para tener todo mas ordenadito
;; instalarlo manualmente por primera vez:
;; M-x package-install RET use-package
(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SISTEMA DE COMLETADO ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Interaz de completado ;;;;;
;; Vertico es una interfáz gráfica de completado (front-end)
;; vertical minimalista y modular fácil de extender (es
;; una alternativa al UI de helm, ivy, selectrum, ido,
;; icomplete, etc.)
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;;;;; Estilo de completado ;;;;;;
;; El estilo de completado (back-end) determina cómo tratar la
;; búsqueda para generar conincidencias. Existen varios
;; estilos: basic, flex, substing, etc.
;; Orderless es un estilo que divide un partrón en
;; componentes separados por espacios y que genera coincidencias
;; con todos los componentes en cualquier orden
(use-package orderless
  :ensure t
  ;; se indican los estilos a usar, se usa basic de fallback
  :custom
  (completion-styles '(orderless basic)))

;; marginalia agrega anotaciones al minibuffer sobre
;; shortcuts y descripciones
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; embark implementa la utilidad de acceder a menus
;; contextuales (i.e., que dependen del"target" donde estamos,
;; por ejemplo, la selección acual)
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h b" . embark-bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;;; PAQUETES UTILIDADES ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :ensure nil
  ;; cambiar intérprete de python a python3 (como se llama en debian 11)
  :config
  (setq python-shell-interpreter "python3"))

;; implementa ideas del método zettelkasten para tomar notas
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/Zettel")
  (org-roam-db-autosync-mode)
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n b" . org-roam-buffer-toggle))
  :config
  ;; configurar cómo se despliega el buffer de info del nodo
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
  ;; configurar cómo se ve el buffer de info del nodo
  ;; (add-hook 'org-roam-mode-hook 'org-variable-pitch-minor-mode)
  (add-hook 'org-roam-mode-hook 'variable-pitch-mode)
  ;; definir los templates de captura
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n")
	   :unnarrowed t)
	  ("r" "reference" plain "%?"
	   :if-new (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n")
	   :unnarrowed t)
	  ("a" "article" plain "%?"
	   :if-new (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n")
	   :unnarrowed t)
	  ("s" "slipbox" entry "* TODO ${title} \n%?"
	   :target (node "slipbox")
	   :unnarrowed t)
	  ))
  
  ;; método para obtener el tipo (nombre de directorio) del nodo
  ;; tomado de https://jethrokuan.github.io/org-roam-guide/
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
	  (file-name-directory
	   (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  
  ;; agregar el tipo al template de completado
  (setq org-roam-node-display-template
	(concat "${type:12} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))
  
;; interfaz gráfica para navegar org-roam en navegador web
(use-package org-roam-ui
  :ensure t
  ;(:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam)

;; paquete para adminsitrar bibliografías bibtex
(use-package citar
  :ensure t
  :custom
  ;; archivos con la bibliografia
  (citar-bibliography '("~/Zettel/ref-all.bib"))
  ;; usar citar como procesador de org-cite
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  ;; refrescar la cache cuando hay cambios en el archivo bib
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
  (setq
   ;; abrir embark en el punto en lugar de la acción predetermiada
   citar-at-point-function 'embark-act
   ;; directorio de notas (se delega esto a una función personalizada
   ;; ver abajo)
   citar-notes-paths '("~/Zettel/reference")
   ;; para mayor consistencia con org-roam y control del
   ;; template se utiliza una función personalizda
   citar-open-note-function 'b3m3bi/org-roam-edit-cite-node))

;; permite abrir archivos con aplicación externa indicada
(use-package openwith
  :ensure t
  :config
  (openwith-mode t)
  (setq openwith-associations
	'(("\\.pdf\\'" "okular" (file)))))

;; guara la historia del minibuffer en el archivo
;; ~/.emacs.d/history y la utiliza en la siguiente
;; sesión
(use-package savehist
  :init
  (savehist-mode))

;; renderizar citas y bibliografias en CSL
(use-package citeproc
  :ensure t
  :config
  (setq org-cite-csl-locales-dir "~/.emacs.d/straight/repos/org/etc/csl")
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

;; implementación de repgrip en emacs que permite buscar
;; recursivamente en el contenido de archivos
(use-package rg
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CONFIGURACIÓN DE ORG-MODE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :config
  ;; se activan lenguajes para babel
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((python  . t)
				 (dot . t)
				 ;(ipython . t)
				 ))
  ;; mostrar las imágenes después de ejecutar un bloque de código
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  ;; iniciar con modo de identación virtual
  (setq org-startup-indented t)
  ;; iniciar con secciones sin desplegar
  (setq org-startup-folded 'content)
  ;; permitir cambiar el tamaño de las imágenes en el preview
  ;; usando una clave (e.g., #+ATTR_ORG: :width 300px)
  (setq org-image-actual-width nil)
  ;; cambiar tamaño de latex preview
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.25))
  ;; definir aplicaciones para abrir distintos tipos de archivos
  (setq org-file-apps
	'((auto-mode . emacs)
	  ("\\.x?html?\\'" . "firefox %s")
	  ("\\.pdf\\'" . "okular \"%s\"")))
  ;; resalta los bloques latex en org
  (setq org-highlight-latex-and-related '(native))
  ;; mostrar imágenes al abrir un archivo org
  (add-hook 'org-mode-hook 'org-toggle-inline-images)
  ;; se esconden los marcadores de enfasis
  (setq org-hide-emphasis-markers t)
  ;; formatear subíndices y superíndices en WYSIWYM (what you see is what you mean)
  (setq org-pretty-entities t
	org-pretty-entities-include-sub-superscripts t)
  ;; cambiar ancho de columnas
  (setq-default fill-column 75)
  ;; definir archivo de bibliografía global
  (setq org-cite-global-bibliography '("~/Zettel/ref-all.bib"))
  ;; definir directorio de org y archivos de notas
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-refile-targets '(("~/org/gdt.org" :maxlevel . 3)))
  ;; template de slipbox
  (setq org-capture-templates
	'(("s" "Slipbox" entry (file "~/Zettel/inbox.org") "* %?\n")
	  ("t" "Todo entry" entry (file "~/org/inbox.org") "* TODO %?"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TEMAS Y APARIENCIA ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package modus-themes
  :ensure t
  ;(:host github :repo "protesilaos/modus-themes" :branch "main" :files ("*.el"))
  :init
  (setq modus-themes-org-blocks 'gray-background)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  ;(setq modus-themes-mixed-fonts t)
  :bind ("<f5>" . modus-themes-toggle))

;; permite mezclar fuentes variables (proporcionales) y mono (fijas)
(use-package mixed-pitch
  :ensure t
  :config
  ;; no cambiar el cursor-style
  (setq mixed-pitch-variable-pitch-cursor nil)
  ;; cambiar también el tamaño de fuente
  (setq mixed-pitch-set-height t))

;; Este podría sustituir a mixed-pitch mode
;; (solo que solo funciona en orgmode a diferencia del
;; otro que funciona también en latex, markdown, info, etc)
(use-package org-variable-pitch
  :ensure t
  :config
  (add-hook 'after-init-hook #'org-variable-pitch-setup))

;; modo para centrar el texto y hacer wrapping 
;; (este puede sustituir a visual-fill-column)
(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 85)
  (add-hook 'org-mode-hook 'olivetti-mode))

;; definir fuente global por defecto
(set-face-attribute 'default nil :family "Hack" :height 112)

;; definir valores de la fuente variable (proporcional)
;;(set-face-attribute 'variable-pitch nil :family "Arial" :height 1.1)
(set-face-attribute 'variable-pitch nil :family "Arial" :height 1.1)

;; definir valores de la fuente fija (mono)
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0)
;;(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 110)

;; definir fuente que usa org-variable-pitch
(set-face-attribute 'org-variable-pitch-fixed-face nil :family "Hack" :height 0.9)

;; Resalta momentaneamente la línea donde esta el point
;; después ejecutar las funciones en `pulsar-pulse-functions'
(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode 1)
  (setq pulsar-face 'pulsar-yellow
	pulsar-pulse-on-window-change t
	pulsar-delay 0.05)
  (setq pulsar-pulse-functions
	'(recenter-top-bottom
	  move-to-window-line-top-bottom
          reposition-window
          ;; bookmark-jump
          ;; other-window
          ;; delete-window
          ;; delete-other-windows
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          ;; windmove-right
          ;; windmove-left
          ;; windmove-up
          ;; windmove-down
          ;; windmove-swap-states-right
          ;; windmove-swap-states-left
          ;; windmove-swap-states-up
          ;; windmove-swap-states-down
          ;; tab-new
          ;; tab-close
          ;; tab-next
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading
	  end-of-buffer
	  beginning-of-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; FUNCIONES PERZONALIZADAS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Abrir init file
(defun b3m3bi/open-init-file ()
  "Open init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "<XF86Favorites>") 'b3m3bi/open-init-file)

;; Abrir gtd file
(defun b3m3bi/open-gtd-file ()
  "Open gtd file."
  (interactive)
  (find-file "~/org/gtd.org"))
(global-set-key (kbd "C-<XF86Favorites>") 'b3m3bi/open-gtd-file)

;; Abrir inbox file
(defun b3m3bi/open-inbox-file ()
  "Open gtd file."
  (interactive)
  (find-file "~/org/inbox.org"))
(global-set-key (kbd "M-<XF86Favorites>") 'b3m3bi/open-inbox-file)

;; Abrir o agregar notas literarias. Usa org-roam de backend
;; y depende de citar. Remplaza la función `citar-open-note-function'.
(defun b3m3bi/org-roam-edit-cite-node (key entry)
  "Si la nota de KEY existe la abre con `find-file' y si
no existe crea un nodo de tipo reference usando `org-roam-capture-' llenando
un template con la información en ENTRY"
  ;; se obtiene el nombre de la nota y se revisa si existe
  ;; el archivo (que ningún elemento de VARLIST sea nil)
    (if-let
	((file (citar-file--get-note-filename key
						   citar-notes-paths
						   citar-file-note-extensions))
	 (file-exists (file-exists-p file)))
	;; si se obtuvo el nombre del archivo y si existe se abre
	(find-file file)
      ;; si no existe el archivo se crea un nodo nuevo
      (let
	  ;; se define el título de la nota
	  ((title (citar--format-entry-no-widths entry "Notes on ${author editor}, ${title}")))
	;; se crea nodo
	(org-roam-capture-
       :info (list :citekey key)
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file)
       :templates '(("r" "reference" plain "%?"
		     :if-new (file+head "reference/${citekey}.org"
					":PROPERTIES:
:ROAM_REFS: @${citekey}
:END:
#+title: ${title} \n")
		   :unnarrowed t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; COSAS NUEVAS PARA PROBAR ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; soporte para usar el kernel de jupyter en org-mode
;; ;; este paquete depende de cl y saca la notificación
;; ;; en el inicio, además se ve que no es mantenido.
;; ;; Explorar otras alternativas e.g., EIN
;; (use-package ob-ipython
;;   :ensure t)

;; autocompletado (no suar con corfu)
;;(use-package company
;;  :ensure t)

;; ;; autocompletado para python (no usar con company)
;; (use-package jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup))

;; corfu es un front-end para autocompletado (da
;; la interfaz y funciones útiles para autocompletado)
;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-auto t)
;;   :init
;;   (corfu-global-mode))

;; ;; saca las ventanitas con la documentación al estilo de vscode
;; (use-package corfu-doc
;;   :ensure
;;   (:host github :repo "galeo/corfu-doc" :branch "main" :files ("*.el"))
;;   :config
;;   (add-hook 'corfu-mode-hook #'corfu-doc-mode))

;; eglot es un cliente de lsp (language service protocol)
;; para dar funcionalidad de autocompletado y otros.
;; Cada lsp se debe instalar y se puede configurar de forma
;; independiente desde fuera de emacs
;; (use-package eglot
;;   :ensure t)

;; emula el efecto de auto-fill-column (que divide automáticamente los
;; párrafos de acuerdo al valor de fill-column)
;; (use-package visual-fill-column
;;   :ensure t
;;   :init
;;   (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
;;   (setq visual-fill-column-center-text t))

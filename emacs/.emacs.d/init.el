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
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((eglot (styles orderless)))))

;; marginalia agrega anotaciones al minibuffer sobre
;; shortcuts y descripciones
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; ;; embark implementa la utilidad de acceder a menus
;; ;; contextuales (i.e., que dependen del"target" donde estamos,
;; ;; por ejemplo, la selección acual). Es como llamar un prefijo
;; ;; de un atajo pero automáticamente.
;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act)
;;    ("M-." . embark-dwim)
;;    ("C-h b" . embark-bindings)))

(require 'embark)

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
  (setq org-roam-directory "~/org/Zettel")
  (org-roam-db-autosync-mode)
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
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
  ;; (key description type template_of_capture(e.g.,lista,heading) template_file)
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}")
	   :unnarrowed t
	   :empty-lines-before 1)
	  ("r" "reference" plain "%?"
	   :if-new (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}")
	   :unnarrowed t
	   :empty-lines-before 1)
	  ("a" "article" plain "\n%?"
	   :if-new (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}")
	   :unnarrowed t)
	  ("p" "project" plain "\n%?"
	   :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}")
	   :unnarrowed t
	   :empty-lines-before 1)
	  ("i" "inbox Zettel" entry "* TODO ${title} \n%?"
	   :target (node "inbox Zettel")
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
  
;; ;; paquete para adminsitrar bibliografías bibtex
;; (use-package citar
;;   :ensure t
;;   :custom
;;   ;; archivos con la bibliografia
;;   (citar-bibliography (list (concat org-roam-directory "/ref-all.bib")))
;;   ;; usar citar como procesador de org-cite
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar)
;;   :config
;;   ;; refrescar la cache cuando hay cambios en el archivo bib
;;   (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
;;   (setq
;;    ;; abrir embark al ejecutar `org-open-at-point (C-c C-o)
;;    citar-at-point-function 'embark-act
;;    ;; directorio de notas
;;    citar-notes-paths (list (concat org-roam-directory "/reference"))
;;    ;; para mayor consistencia con org-roam y control del
;;    ;; template se utiliza una función personalizda
;;    citar-open-note-functions '(b3m3bi/org-roam-edit-cite-node)))

;; usar embark para ejecutar acciones contextuales en el punto
;; (use-package citar-embark
;;   :after citar embark
;;   :no-require
;;   :config (citar-embark-mode))

;; permite abrir archivos con aplicación externa indicada
;; (use-package openwith
;;   :ensure t
;;   :config
;;   (openwith-mode t)
;;   (setq openwith-associations
;; 	'(("\\.pdf\\'" "okular" (file)))))

;; guarda la historia del minibuffer en el archivo
;; ~/.emacs.d/history y la utiliza en la siguiente
;; sesión
(use-package savehist
  :init
  (savehist-mode))

;; renderizar citas y bibliografias usano CSL
(use-package citeproc
  :ensure t
  :config
  (setq org-cite-csl-locales-dir "~/.emacs.d/straight/repos/org/etc/csl")
  ;; directorio de estilos csl (se pueden descargar con
  ;; la interfáz gráfica de Zotero)
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

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
  (setq org-hide-emphasis-markers nil)
  ;; formatear subíndices y superíndices en WYSIWYM (what you see is what you mean)
  (setq org-pretty-entities t
	org-pretty-entities-include-sub-superscripts nil)
  ;; incluir listas a ciclo de colapso de entradas
  (setq org-cycle-include-plain-lists 'integrate)
  ;; cambiar ancho de columnas
  (setq-default fill-column 75)
  ;; archivos disponbiles para construir agenda
  (setq org-agenda-files '("GTD/gtd.org"))
  ;; definir archivo de bibliografía global
  (setq org-cite-global-bibliography (list (concat org-roam-directory "/ref-all.bib")))
  ;; definir directorio de org y archivos de notas
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; archivos disponibles para hacer refiles
  (setq org-refile-targets (list
			    (cons (concat org-directory "/GTD/gtd.org")
				  (cons :maxlevel 3))
			    (cons (concat org-directory "/GTD/someday.org")
				  (cons :maxlevel 3))))
  (setq org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)
  ;; templates
  (setq org-capture-templates
	'(("g" "GTD inbox" entry (file "GTD/inbox.org") "* TODO %?")
	  ("z" "Zettel inbox" entry (file "Zettel/inbox.org") "* TODO %?"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TEMAS Y APARIENCIA ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package modus-themes
;;  :ensure nil ;; para usar el paquete construido por default en emacs 28.1
;;  :init
;;  (setq modus-themes-org-blocks 'gray-background)
;;  (modus-themes-load-themes)
;;  :config
;;  (modus-themes-load-operandi)
;;  ;(setq modus-themes-mixed-fonts t)
;;  :bind ("<f5>" . modus-themes-toggle))

;; se configura el tema y luego se carga
;; poner en negritas e itálicas algunos elementos como
;; nombres de funciones y comentarios
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-mode-line '(borderless))
(setq modus-themes-completions 'moderate)
(setq modus-themes-hl-line '(accented))
(setq modus-themes-subtle-line-numbers t)
(setq modus-themes-paren-match '(bold))
(setq modus-themes-region '(bg-only accented))
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-org-agenda
      '((header-block . (scale-title))
	(header-date . (bold-today grayscale))
	(event . (italic))))
(load-theme 'modus-operandi)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)


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
  (setq olivetti-body-width 90)
  (add-hook 'org-mode-hook 'olivetti-mode))

;; definir fuente global por defecto
(set-face-attribute 'default nil :family "Hack" :height 112)

;; definir valores de la fuente variable (proporcional)
(set-face-attribute 'variable-pitch nil :family "Liberation Sans" :height 1.1 :weight 'normal)

;; definir valores de la fuente fija (mono)
(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0)

;; definir fuente que usa org-variable-pitch
(set-face-attribute 'org-variable-pitch-fixed-face nil :family "Hack" :height 1.0)

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
  (find-file (concat org-directory "/GTD/gtd.org")))
(global-set-key (kbd "C-<XF86Favorites>") 'b3m3bi/open-gtd-file)

;; Abrir inbox file
(defun b3m3bi/open-inbox-file ()
  "Open gtd file."
  (interactive)
  (find-file (concat org-directory "/GTD/inbox.org")))
(global-set-key (kbd "M-<XF86Favorites>") 'b3m3bi/open-inbox-file)

;; Abrir o agregar notas literarias. Usa org-roam de backend
;; y depende de citar. Remplaza la función `citar-open-note-function'.
;; basado en https://jethrokuan.github.io/org-roam-guide/
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
       :templates '(("r" "reference" plain "%?"
		     :if-new (file+head "reference/${citekey}.org"
					":PROPERTIES:
:ROAM_REFS: @${citekey}
:END:
#+title: ${title}")
		     :unnarrowed t
		     :empty-lines-before 1))
       :info (list :citekey key)
       :node (org-roam-node-create :title title)
       :props '(:finalize find-file)))))

(defun last-saturday-of-month (date)
  "Return `t` if DATE is the last saturday of the month."
(let*
    ((day (calendar-extract-day date))
     (month (calendar-extract-month date))
     (year (calendar-extract-year date))
     (last-day-month (calendar-last-day-of-month month year))
     (num-next-day-of-week (+ day 7)))
  (and
   ;; true if is saturday
   (eq (calendar-day-of-week date) 6)
   ;; true if next
   (> num-next-day-of-week last-day-month))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; COSAS NUEVAS PARA PROBAR ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; soporte para usar el kernel de jupyter en org-mode
;; ;; este paquete depende de cl y saca la notificación
;; ;; en el inicio, además se ve que no es mantenido.
;; ;; Explorar otras alternativas e.g., EIN
;; (use-package ob-ipython
;;   :ensure t)

;; saca las ventanitas con la documentación al estilo de vscode
;; (use-package corfu-doc
;;   :ensure t
;;   :config
;;   (add-hook 'corfu-mode-hook #'corfu-doc-mode)
;;   (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
;;   (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
;;   (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))

;; interfaz gráfica para navegar org-roam en navegador web
;; (use-package org-roam-ui
;;   :ensure t)

;; resalta la línea en la que se está en un documento
;(global-hl-line-mode)

;; interfáz para buscar notas se actualiza directamente
;; (pero es medio lento)
;; (use-package deft
;;   :ensure nil
;;   :after org
;;   :bind ("C-c n d" . deft)
;;   :custom
;;   (deft-default-extension "org")
;;   (deft-directory (concat org-roam-directory "/main"))
;;   (deft-recursive nil)
;;   (deft-use-filter-string-for-filename))

;; implementación de repgrip en emacs que permi;; te buscar
;; ;; recursivamente en el contenido de archivos
;; (use-package rg
;;   :ensure t
;;   :bind ("C-c n l" . b3m3bi/rg-lt-search-zettel)
;;   :config
;;   (rg-define-search b3m3bi/rg-lt-search-zettel
;;     "Search in zettel files regex"
;;     :query ask
;;     :format literal
;;     :files "org"
;;     :dir org-roam-directory
;;     :menu ("Custom" "z" "zettel")))

;; según entiendo este paquete hace muchas acciones con el sistema
;; complete-read. Por ejemplo, permite hacer búsquedas desde el
;; sistema de completado y ejecutar a la vez en vivo otras funciones
(use-package consult
  :ensure t
  :bind ("C-c n r" . b3m3bi/org-roam-consult-rg))

(defun b3m3bi/org-roam-consult-rg ()
  "Consult ripgrep on org-roam-directory."
  (interactive)
  (consult-ripgrep org-roam-directory))

;; corfu es un front-end para autocompletado en el buffer
;; (da la interfaz y funciones útiles para autocompletado)
;; funciona usando dabbrevs (C-/) y capfs. Y también puede
;; usar lo que se recibe del servidor LSP
(use-package corfu
  :ensure t
  :init
  :config
  (setq corfu-auto t)
  :hook
  ((python-mode . corfu-mode)
   (lisp-mode . corfu-mode)))

;; ;; https://github.com/minad/corfu/wiki
;; (use-package lsp-mode
;;   :custom
;;   (lsp-completion-provider :none)
;;   :init
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))) 
;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion)
;;   (python-mode . lsp-completion-mode-hook)
;;   :commands lsp)

;; cliente lsp configurado para funcionar con corfu
;; (use-package lsp-mode
;;   :custom
;;   (lsp-completion-provider :none)
;;   :init
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
;;   :config
;;   ;; configuraciones para mejorar el desempeño (revisar con M-x lsp-doctor)
;;   ;; ver https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (setq gc-cons-threshold 1600000
;; 	read-process-output-max (* 1024 1024))
;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion)
;;   (python-mode . lsp)
;;   :commands lsp)

(use-package eglot
  :ensure t
  :config 
  (add-hook 'python-mode-hook 'eglot-ensure))

(add-to-list 'load-path "~/.emacs.d/manual-packages/org-krita")

(use-package org-krita
  :ensure nil
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))

;; (use-package display-line-numbers
;;   :ensure nil
;;   :hook
;;   (emacs-lisp-mode . display-line-numbers-mode)
;;   (python-mode . display-line-numbers-mode))

(use-package code-cells
  :ensure nil
  :bind
  (("C-c C-w" . code-cells-eval)
   ("M-p" . code-cells-backward-cell)
   ("M-n" . code-cells-forward-cell)
   ("M-N" . code-cells-move-cell-down)
   ("M-P" . code-cells-move-cell-up)))

;; probar para ver si sustituye a org-roam
;; una ventaja muy buena es que no descuida el nombre del archivo
;; lo que lo hace más portable (fuera de eso creo que necesita más configuración
;; que org-roam)
(use-package denote
  :ensure nil
  :config
  (setq denote-directory "~/Pruebas/notas/")
  (setq denote-known-keywords
	'("main" "reference" "project" "article" "computación" "ciencia" "matemáticas"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil)
  (setq denote-allow-multi-word-keywords nil)
  (add-hook 'dired-mode-hook #'denote-dired-mode)

 (with-eval-after-load 'org-capture
       (require 'denote-org-capture)
       (setq denote-org-capture-specifiers "%l\n%i\n%?")
       (add-to-list 'org-capture-templates
                    '("n" "New note (with denote.el)" plain
                      (file denote-last-path)
                      #'denote-org-capture
                      :no-save t
                      :immediate-finish nil
                      :kill-buffer t
                      :jump-to-captured t))))

;;;; intento de hacer que dired no me muestre los archivos no relevantes
;; como lo que terminan en ~ o #
;; (use-package dired-x
;;   :ensure nil
;;   :after dired
;;   :config
;;   (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook
	  (lambda () (dired-omit-mode 1) ))

;; ;; probando cómo se configura ahora citar que lo cambiaron todo :(
;; (use-package citar
;;   :config
;;   (setq citar-bibliography (list (concat org-roam-directory "/ref-all.bib")))
;;   (setq citar-at-point-function 'embark-act)
;;   (setq org-cite-insert-processor 'citar)
;;   (setq org-cite-follow-processor 'citar)
;;   (setq org-cite-activate-processor 'citar))

;; (use-package citar-embark
;;   :after citar embark
;;   :no-require
;;   :config (citar-embark-mode))

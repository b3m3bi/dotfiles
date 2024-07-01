;;; Configuración general

;; se quita el mensaje de inicio
(setq inhibit-startup-message t)

;; se quita toolbar, menubar y scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; flashes en lugar de sonido feo
(setq visible-bell t)

;; cambiar interprete de python a python3 (como se llama en debian 12)
(setq python-shell-interpreter "python3")

;; guarda la historia del minibuffer en el archivo
;; ~/.emacs.d/history y la utiliza en sesiones futuras
(savehist-mode 1)

;; activar modo de precisión de scroll
(pixel-scroll-precision-mode 1)

;; cambio el directorio de backups de emacs
(setq backup-directory-alist '(( "." . "~/.emacs.d/backups")))

;;;; custom
;; se mandan las ediciones de customize a otro archivo y se cargan
;; pero procurar no usar custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; package.el
(require 'package)
;; se agrega el repo de melpa
;; para actualizar la lista de paquetes: M-x package-refresh-contents RET
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; se le pide a package que cargue y active los paquetes, así ya no se tiene
;; que hacer `require a cada paquete
;; (package-initialize)		 

;;; lista de paquetes que uso
(setq package-list 
      '(
	vertico
	marginalia
	corfu
	orderless
	code-cells
	ess
        modus-themes
	denote
	jinx
	rainbow-mode
	virtualenvwrapper
	org-variable-pitch
	citar			  ; melpa
	citeproc		  ; melpa
	olivetti		  ; melpa
	markdown-mode		  ; melpa nongnu
	yaml-mode		  ; melpa nongnu
	openwith
	))
;; package registra los paquetes installados en `package-selected-packages (en
;; una vairable en custom.init) por lo que para (re)installar los paquetes
;; con `package-install-selected-packages o para eliminar automáticamente paquetes
;; no usados con `package-autoremove se puede usar dicha variable:
;; (setq package-selected-packages package-list)
;; (package-install-selected-packages)
;; (package-autoremove)

;;; vc
;; seguir symlinks de repos bajo control de versión sin tener que confirmar
(setq vc-follow-symlinks nil)

;;; vertico
;; Vertico es una interfáz gráfica de completado (front-end)
;; vertical minimalista y modular fácil de extender (es
;; una alternativa al UI de helm, ivy, selectrum, ido,
;; icomplete, etc.)
(vertico-mode 1)
(vertico-mouse-mode 1)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; marginalia agrega anotaciones al minibuffer sobre
;; shortcuts y descripciones
(marginalia-mode 1)
;; (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

;; corfu es un front-end para autocompletado en el buffer
;; funciona usando dabbrevs (C-/) y capfs. Y también puede
;; usar lo que se recibe de un servidor LSP
(setq corfu-auto t)
(add-hook 'python-mode-hook #'corfu-mode)
(add-hook 'emacs-lisp-mode-hook #'corfu-mode)
(add-hook 'R-mode-hook #'corfu-mode)

;;; Estilo de completado

;; El estilo de completado (back-end) determina cómo tratar la
;; búsqueda para generar conincidencias. Existen varios
;; estilos: basic, flex, substing, etc.
;; Orderless es un estilo que divide un partrón en
;; componentes separados por espacios y que genera coincidencias
;; con todos los componentes en cualquier orden
(setq completion-styles '(orderless basic))
;; en el caso de búsqueda de archivos se usa otro estilo
;; que permite hacer búsquedas como /u/l/sh para /user/local/share,
;; también se debe modificar la búsqueda de eglot para que use orderless
;; en el autocompletado
(setq completion-category-overrides
      '((eglot (styles orderless))
	(file (styles basic partial-completion))))

;;; Utilidades de completado

;;;; eglot
;; Eglot es un cliente de servidores LSP https://github.com/joaotavora/eglot
;; los servidores se instalan y configuran aparte (ver notas-config)
;; se activa eglot en varios modos:
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'R-mode-hook #'eglot-ensure)

;;;;; oc-csl ;;;;;;

;; procesador de exportación de citas con soporte para CSL
;; depende de citeproc-el https://github.com/andras-simonyi/citeproc-el

(require 'oc-csl)
;; directorio de los locales
(setq org-cite-csl-locales-dir "~/.emacs.d/locales")
;; directorio de estilos csl (se pueden descargar con
;; la interfáz gráfica de Zotero)
(setq org-cite-csl-styles-dir "~/Zotero/styles")

;;;;; citeproc-biblatex ;;;;;;

(require 'oc-biblatex)
;; usar biblatex además requiere que se tenga instalado texlive-bibtex-extra (sudo apt install texlive-bibtex-extra) y biber (sudo apt install biber)

;;;;; markdown-mode ;;;;;

(setq markdown-enable-math t)

;;;;; code-cells ;;;;;;

(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map (kbd "C-c C-w") 'code-cells-eval)
    (define-key map (kbd "M-]") 'code-cells-forward-cell)
    (define-key map (kbd "M-[") 'code-cells-backward-cell)))
(add-hook 'python-mode-hook #'code-cells-mode)

;;;;; denote ;;;;;
(setq denote-directory (expand-file-name "~/zttlkstn"))
(setq org-cite-global-bibliography (list (concat denote-directory "/ref-all-2.bib")))
(setq denote-file-type 'org)
(setq denote-known-keywords '("matemáticas" "computación" "biología" "física"
			      "economía" "academia" "ilustración" "productividad"))
(setq denote-excluded-keywords-regexp "[0-9]")
(global-set-key (kbd "C-c n n") #'denote-subdirectory)
(global-set-key (kbd "C-c n r") #'denote-rename-file)
(global-set-key (kbd "C-c n C-r") #'denote-rename-file-using-front-matter)
(global-set-key (kbd "C-c n f") #'denote-open-or-create)
(global-set-key (kbd "C-c n i") #'denote-link-or-create)
(global-set-key (kbd "C-c n b") #'denote-link-backlinks)
(setq denote-excluded-directories-regexp ".*img.*\\|.*ltx.*")

(add-hook 'dired-mode-hook #'denote-dired-mode)
(setq xref-search-program 'ripgrep)

;; agregar paquete para formatear completing-read
;; (add-to-list 'load-path (expand-file-name "denote-completing-format" "~/.emacs.d/b3m3bi-packages/"))
;; (require 'denote-completing-format)
;; (global-set-key (kbd "C-c n f") #'denote-completing-format-open-or-create)
;; (global-set-key (kbd "C-c n i") #'denote-completing-format-link-or-create)
;; (setq denote-completing-format-subdir-width 20)
;; (setq denote-completing-format-keywords-width 25)
;; (setq denote-completing-format-create-function 'denote-subdirectory)
 
;;;;; citar ;;;;;
(setq citar-bibliography org-cite-global-bibliography)
(setq org-cite-insert-processor 'citar)
(setq org-cite-follow-processor 'citar)
(setq org-cite-activate-processor 'citar)
(setq citar-open-prompt t)
(require 'citar-file)
(add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))

;;;;; virtualenvwrapper ;;;;;

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/Envs")

;;;;;; jinx ;;;;;
;; corrector ortográfico que corrige todo lo que se ve del buffer
(require 'jinx)
(setq jinx-languages "es_MX.UTF-8")
(setq ispell-dictionary "es")
;; (keymap-global-set "<remap> <ispell-word>" #'jinx-correct)

;; ;;;;; latex ;;;;;
;; ;; se agrega una nueva clase para exportar en clase "book" pero sin considerar "part"
;; (require 'ox-latex)
;; (add-to-list 'org-latex-classes
;; 	     '("tesis" "\\documentclass[11pt]{tesis}"
;; 	       ("\\chapter{%s}" . "\\chapter*{%s}")
;; 	       ("\\section{%s}" . "\\section*{%s}")
;; 	       ("\\subsection{%s}" . "\\subsection*{%s}")
;; 	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CONFIGURACIÓN DE ORG-MODE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; se activan lenguajes para babel
(org-babel-do-load-languages 'org-babel-load-languages
			     '((python  . t)
			       (R . t)
			       (emacs-lisp . t)))
;; cambio de nombre el comando usado para correr python
(setq org-babel-python-command "python3")
;; mostrar las imágenes después de ejecutar un bloque de código
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
;; iniciar con modo de identación virtual
(setq org-startup-indented t)
;; iniciar con secciones desplegadas (nótese que `showeverything
;; interfiere con variable `org-hide-block-startup)
(setq org-startup-folded 'nofold)
;; colapsar bloques de código
(setq org-hide-block-startup nil)
;; permitir cambiar el tamaño de las imágenes en el preview
;; usando una clave (e.g., #+ATTR_ORG: :width 300px)
(setq org-image-actual-width nil)
;; cambiar tamaño de latex preview
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.25))
;; definir aplicaciones para abrir distintos tipos de archivos
(setq org-file-apps
	'((auto-mode . emacs)
	  ("\\.pdf\\'" . "okular \"%s\"")))
;; resalta los bloques latex en org
(setq org-highlight-latex-and-related '(native))
;; necesario para exportar formulas de mate a odt
(setq org-latex-to-mathml-convert-command "latexmlmath '%i' --presentationmathml=%o")
;; aplicar el aplastar listas al tamaño indicado en <4> al iniciar
(setq org-startup-shrink-all-tables t)
;; evitar que tab y enter tengan significado especial en las tablas
(setq org-table-auto-blank-field nil)
;; archivos disponibles para construir agenda
(setq org-agenda-files '("gtd.org" "agenda.org"))
;; definir directorio de org y archivos de notas
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; archivos disponibles para hacer refiles
(setq org-refile-targets (list
			    (cons (concat org-directory "/GTD/gtd.org")
				  (cons :maxlevel 3))
			    (cons (concat org-directory "/GTD/someday.org")
				  (cons :maxlevel 3))
			    (cons (concat org-directory "/GTD/habits.org")
				  (cons :maxlevel 3))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;; templates
(setq org-capture-templates
      '(("g" "GTD inbox" entry (file "GTD/inbox.org") "* TODO %?")
	;; ("z" "Zettel inbox" entry (file "Zettel/inbox.org") "* TODO %?")
	))

(eval-after-load "org"
  '(require 'ox-md nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TEMAS Y APARIENCIA ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; modus themes ;;;;;;

;; se configura el tema y luego se carga
;; poner en negritas e itálicas algunos elementos como
;; nombres de funciones y comentarios
(require 'modus-themes)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
	(border-mode-line-inactive bg-mode-line-inactive)
	(fringe unspecified)))
(load-theme 'modus-operandi :no-confirm)
(global-set-key (kbd "<f5>") #'modus-themes-toggle)

;;;;;;; olivetti ;;;;;;

;; modo para centrar el texto y hacer wrapping
(setq olivetti-body-width 80)
(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'markdown-mode-hook #'olivetti-mode)

;;;;;;; fuentes ;;;;;;

;; definir fuente global por defecto
(set-face-attribute 'default nil :family "JetBrains Mono" :height 110 :weight 'light)
;; definir valores de la fuente variable (proporcional)
(set-face-attribute 'variable-pitch nil :family "Liberation Serif" :height 1.2 :weight 'normal)
;; definir valores de la fuente fija (mono)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 110 :weight 'light)

;;;;;; org-variable-pitch ;;;;;;

(require 'org-variable-pitch)
;; definir fuente que usa org-variable-pitch
(set-face-attribute 'org-variable-pitch-fixed-face nil :family "JetBrains Mono" :height 110 :weight 'light)
(add-to-list 'org-variable-pitch-fixed-faces 'font-lock-comment-face)
(setq org-variable-pitch-fontify-headline-prefix nil)

;; (add-to-list 'load-path (expand-file-name "org-variable-pitch-custom" "~/.emacs.d/b3m3bi-packages/"))
;; (require 'org-variable-pitch-custom)
;; (add-to-list 'org-variable-pitch-custom-group 'org-document-title)
;; (add-to-list 'org-variable-pitch-custom-group 'org-document-info)

;;;;;; linum-mode ;;;;;;
(add-hook 'python-mode-hook #'display-line-numbers-mode)
(add-hook 'R-mode-hook #'display-line-numbers-mode)
(add-hook 'lisp-mode-hook #'display-line-numbers-mode)

;;;;;; openwith ;;;;;;
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "okular" (file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; FUNCIONES PERZONALIZADAS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun last-saturday-of-month (date)
;;   "Return `t` if DATE is the last saturday of the month."
;; (let*
;;     ((day (calendar-extract-day date))
;;      (month (calendar-extract-month date))
;;      (year (calendar-extract-year date))
;;      (last-day-month (calendar-last-day-of-month month year))
;;      (num-next-day-of-week (+ day 7)))
;;   (and
;;    ;; true if is saturday
;;    (eq (calendar-day-of-week date) 6)
;;    ;; true if next
;;    (> num-next-day-of-week last-day-month))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; función para activar eglot en `org-edit-special tomada de
;; ;; https://github.com/joaotavora/eglot/issues/216#issuecomment-1052931508
;; (defun mb/org-babel-edit:python ()
;;   "Edit python src block with lsp support by tangling the block and
;; then setting the org-edit-special buffer-file-name to the
;; absolute path. Finally load eglot."
;;   (interactive)
;;   ;; org-babel-get-src-block-info returns lang, code_src, and header
;;   ;; params; Use nth 2 to get the params and then retrieve the :tangle
;;   ;; to get the filename
;;   (setq mb/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))
;;   ;; tangle the src block at point 
;;   (org-babel-tangle '(4))
;;   (org-edit-special)
;;   ;; Now we should be in the special edit buffer with python-mode. Set
;;   ;; the buffer-file-name to the tangled file so that pylsp and
;;   ;; plugins can see an actual file.
;;   (setq-local buffer-file-name mb/tangled-file-name)
;;   (eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; COSAS NUEVAS PARA PROBAR ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Otros paquetes relevantes que he probado:
;; - corfu-doc (ventanitas con la documentación, corfu tiene la variable
;; `corfu-info-documentation' que activa esta función)
;; - rg (implementacion de repgrip en emacs, búsquedas recursivas en contenidos de archivos)
;; - openwith (permite abrir archivos con apliación externa preferida)
;; - org-krita (insertar archivos de krita en org)
;; - citar-embark (utilidades de embark en citar)
;; - mixed-pitch (mezclar fuentes proporcionales y fijas en org, latex, markdow, info)
;; - consult (según entiendo este paquete hace muchas acciones con el sistema
;; complete-read. Por ejemplo, permite hacer búsquedas desde el
;; sistema de completado y ejecutar a la vez en vivo otras funciones
;; que permiten previsualizar la búsqueda)
;; - embark (implementa la utilidad de acceder a menus
;; contextuales (i.e., que dependen del"target" donde estamos,
;; por ejemplo, la selección acual o la palabra en el punto).
;; Es como llamar un prefijo de un atajo pero automáticamente)
;; - embark-consult (integración de embark con consult)

;;;;;; embark ;;;;;;

;; (global-set-key (kbd "C-.") 'embark-act)
;; (global-set-key (kbd "M-.") 'embark-dwim)
;; (global-set-key (kbd "C-h b") 'embark-bindings)

;; función para buscar una palabra en un archivo en mi zettel (depende de consult)
;; (defun b3m3bi/org-roam-consult-rg ()
;;   "Consult ripgrep on `org-roam-directory."
;;   (interactive)
;;   (consult-ripgrep org-roam-directory))
;; (global-set-key (kbd "C-c n r") 'b3m3bi/org-roam-consult-rg)


;;;;; función para cambiar temas ;;;;;
;; (setq themes-to-toggle '(modus-operandi modus-vivendi))
;; ;;(setq themes-to-toggle (custom-available-themes))
;; (defun b3m3bi/toggle-themes ()
;;   "Toggle between themes defined on `themes-to-toggle'."
;;   (interactive)
;;   (if-let* ((themes themes-to-toggle)
;; 	    (number-of-themes (length themes))
;; 	    (current-theme (car custom-enabled-themes))
;; 	    (current-theme-index (cl-position current-theme themes))
;; 	    (next-theme-index (if (eq current-theme-index (- number-of-themes 1))
;; 				  0
;; 				(+ current-theme-index 1)))
;; 	    (next-theme (nth next-theme-index themes)))
;;       (progn
;; 	(mapc #'disable-theme custom-enabled-themes)
;; 	(load-theme next-theme :no-confirm)
;; 	(message "Load: %s theme" next-theme))
;;     (user-error "Error ocurred :(")))

;; (global-set-key (kbd "<f5>") #'b3m3bi/toggle-themes)


;; Cosas para quitar
;;;; org-roam ;;;;;;

;; ;; implementa ideas del método zettelkasten para tomar notas
;; (setq org-roam-v2-ack t)
;; (setq org-roam-directory "~/org/Zettel")
;; (org-roam-db-autosync-mode 1)
;; (define-key global-map (kbd "C-c m f") #'org-roam-node-find)
;; ;; (define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
;; ;; (define-key global-map (kbd "C-c n c") #'org-roam-capture)
;; ;; (define-key global-map (kbd "C-c n b") #'org-roam-buffer-toggle)
;; ;; configurar cómo se despliega el buffer de info del nodo
;; (add-to-list 'display-buffer-alist
;; 	       '("\\*org-roam\\*"
;; 		 (display-buffer-in-direction)
;; 		 (direction . right)
;; 		 (window-width . 0.33)
;; 		 (window-height . fit-window-to-buffer)))
;; ;; definir los templates de captura
;; ;; (key description type template_of_capture(e.g.,lista,heading) template_file/node)
;; (setq org-roam-capture-templates
;; 	'(("d" "default" plain "%?"
;; 	   :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
;; 			      "#+title: ${title}")
;; 	   :unnarrowed t
;; 	   :empty-lines-before 1)
;; 	  ("r" "reference" plain "%?"
;; 	   :if-new (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org"
;; 			      "#+title: ${title}")
;; 	   :unnarrowed t
;; 	   :empty-lines-before 1)
;; 	  ("a" "article" plain "\n%?"
;; 	   :if-new (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
;; 			      "#+title: ${title}")
;; 	   :unnarrowed t)
;; 	  ("p" "project" plain "\n%?"
;; 	   :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
;; 			      "#+title: ${title}")
;; 	   :unnarrowed t
;; 	   :empty-lines-before 1)
;; 	  ("i" "inbox Zettel" entry "* TODO ${title} \n%?"
;; 	   :target (node "inbox Zettel")
;; 	   :unnarrowed t)))

;; ;; método para obtener el tipo (nombre de directorio) del nodo
;; ;; tomado de https://jethrokuan.github.io/org-roam-guide/
;; (cl-defmethod org-roam-node-type ((node org-roam-node))
;;   "Return the TYPE of NODE."
;;   (condition-case nil
;; 	(file-name-nondirectory
;; 	 (directory-file-name
;; 	  (file-name-directory
;; 	   (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;     (error "")))

;; ;; agregar el tipo al template de completado
;; (setq org-roam-node-display-template
;; 	(concat "${type:12} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))



;; (add-to-list 'load-path (expand-file-name "netlogo-mode" "~/.emacs.d/manual-packages"))
;; (require 'netlogo-mode)


;; (require 'julia-repl)
;; (add-hook 'julia-mode-hook 'julia-repl-mode)

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))


;; (define-key python-mode-map (kbd "C-c C-q") 'recompile)
;; 

;; ;;;;; winner-mode ;;;;;

;; ;; historial de ordenamiento de las ventanas (páneles)
;; ;; regresar a config previa con C-c {flechas}
;; (winner-mode 1)

;; ;;;;; dired ;;;;
;; (setq dired-isearch-filenames 'dwim)
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "M-+") 'dired-create-empty-file)
;;   ;; cargar dired-x y ocultar archivos no interesantes
;;   ;; (e.g., backups ~, auto-guardados #)
;;   (require 'dired-x))
;; (add-hook 'dired-mode-hook
;; 	  (lambda ()
;; 	    (dired-omit-mode 1)))
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; ;;;;;; anzu ;;;;;;
;; ;; agrega el número de resultados de una búsqueda en la mode-line
;; (global-anzu-mode 1)

;; ;;;;;;; pulsar ;;;;;;;

;; ;; Resalta momentaneamente la línea donde esta el point
;; ;; después ejecutar las funciones en `pulsar-pulse-functions'
;; ;; color de resaltado
;; (setq pulsar-face 'pulsar-yellow)
;; ;; activar resaltado en todas las funciones que afectan a la ventana activa
;; (setq pulsar-delay 0.05)
;; ;; atajo para relsatar linea actual
;; (global-set-key (kbd "C-c p") 'pulsar-pulse-line)
;; ;; se activa globalmente el modo
;; (pulsar-global-mode 1)

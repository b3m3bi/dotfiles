
;;; Configuración general

;; se quita el mensaje de inicio
(setq inhibit-startup-message t)

;; se quita toolbar, menubar y scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; flashes en lugar de sonido feo
(setq visible-bell t)

;; cambiar interprete de python a python3 (como se llama en debian 11)
(setq python-shell-interpreter "python3")

;; guarda la historia del minibuffer en el archivo
;; ~/.emacs.d/history y la utiliza en sesiones futuras
(savehist-mode 1)

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

;; lista de paquetes que uso
(setq package-list 
      '(
	vertico
	marginalia
	corfu
	orderless
	eglot			  ;va a venir incluido en emacs 29 :)
	pulsar 
	code-cells
	ess
        modus-themes
	citar			  ; melpa
	org-roam		  ; melpa
	citeproc		  ; melpa
	olivetti		  ; melpa
	markdown-mode		  ; melpa nongnu
	yaml-mode		  ; melpa nongnu
	))
;; package registra los paquetes installados en `package-selected-packages (en
;; una vairable en custom.init) por lo que para (re)installar los paquetes
;; con `package-install-selected-packages o para eliminar automáticamente paquetes
;; no usados con `package-autoremove se puede usar dicha variable:
;; (setq package-selected-packages package-list)
;; (package-install-selected-packages)
;; (package-autoremove)

;;; vc
;; seguir symlinks sin tener que confirmar
(setq vc-follow-symlinks nil)

;;; vertico
;; Vertico es una interfáz gráfica de completado (front-end)
;; vertical minimalista y modular fácil de extender (es
;; una alternativa al UI de helm, ivy, selectrum, ido,
;; icomplete, etc.)
(vertico-mode 1)
(vertico-mouse-mode 1)

;; marginalia agrega anotaciones al minibuffer sobre
;; shortcuts y descripciones
(marginalia-mode 1)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

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

;;;; org-roam ;;;;;;

;; implementa ideas del método zettelkasten para tomar notas
(setq org-roam-v2-ack t)
(setq org-roam-directory "~/org/Zettel")
(org-roam-db-autosync-mode 1)
(define-key global-map (kbd "C-c m f") #'org-roam-node-find)
;; (define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
;; (define-key global-map (kbd "C-c n c") #'org-roam-capture)
;; (define-key global-map (kbd "C-c n b") #'org-roam-buffer-toggle)
;; configurar cómo se despliega el buffer de info del nodo
(add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
;; definir los templates de captura
;; (key description type template_of_capture(e.g.,lista,heading) template_file/node)
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
	   :unnarrowed t)))

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
	(concat "${type:12} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;;;;; oc-csl ;;;;;;

;; procesador de exportación de citas con soporte para CSL
;; depende de citeproc-el https://github.com/andras-simonyi/citeproc-el

(require 'oc-csl)
;; directorio de los locales
(setq org-cite-csl-locales-dir "~/.emacs.d/locales")
;; directorio de estilos csl (se pueden descargar con
;; la interfáz gráfica de Zotero)
(setq org-cite-csl-styles-dir "~/Zotero/styles")

;;;;; markdown-mode ;;;;;

(setq markdown-enable-math t)

;;;;; code-cells ;;;;;;

(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map (kbd "C-c C-w") 'code-cells-eval)
    (define-key map (kbd "M-]") 'code-cells-forward-cell)
    (define-key map (kbd "M-[") 'code-cells-backward-cell)))
(add-hook 'python-mode-hook #'code-cells-mode)

;;;;; dired ;;;;
(setq dired-isearch-filenames 'dwim)
;; cargar dired-x y ocultar archivos no interesantes (e.g., backups ~, auto-guardados #)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-+") 'dired-create-empty-file)
  (require 'dired-x))
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-omit-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CONFIGURACIÓN DE ORG-MODE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; se activan lenguajes para babel
(org-babel-do-load-languages 'org-babel-load-languages
			     '((python  . t)
			       (emacs-lisp . t)
				 ;(dot . t)
				 ;(ipython . t)
			       ))
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
;; mostrar imágenes al abrir un archivo org
;; (add-hook 'org-mode-hook 'org-toggle-inline-images)
;; se esconden los marcadores de enfasis
(setq org-hide-emphasis-markers nil)
;; formatear subíndices y superíndices en WYSIWYM (what you see is what you mean)
(setq org-pretty-entities nil)
(setq org-pretty-entities-include-sub-superscripts nil)
;; bloques de codigo
(setq org-fontify-whole-block-delimiter-line t)
;; incluir listas a ciclo de colapso de entradas
(setq org-cycle-include-plain-lists nil)
;; necesario para exportar formulas de mate a odt
(setq org-latex-to-mathml-convert-command "latexmlmath '%i' --presentationmathml=%o")
;; aplicar el aplastar listas al tamaño indicado en <4> al iniciar
(setq org-startup-shrink-all-tables t)
;; evitar que tab y enter tengan significado especial en las tablas
(setq org-table-auto-blank-field nil)
;; archivos disponbiles para construir agenda
(setq org-agenda-files '("GTD/gtd.org" "GTD/agenda.org"))
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
(load-theme 'modus-vivendi :no-confirm)
(global-set-key (kbd "<f5>") #'modus-themes-toggle)

;;;;;;; olivetti ;;;;;;

;; modo para centrar el texto y hacer wrapping
(setq olivetti-body-width 80)
(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'markdown-mode-hook #'olivetti-mode)

;;;;;;; fuentes ;;;;;;

;; definir fuente global por defecto
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110)
;; (set-face-attribute 'default nil :family "Hack" :height 110)
;; (set-face-attribute 'default nil :family "Fira Code" :height 108)
;; (set-face-attribute 'default nil :family "Iosevka Comfy" :height 120)
(set-face-attribute 'default nil :family "JetBrains Mono" :height 110 :weight 'light)

;; definir valores de la fuente variable (proporcional)
(set-face-attribute 'variable-pitch nil :family "Liberation Sans" :height 1.1 :weight 'normal)
(set-face-attribute 'variable-pitch nil :family "Liberation Serif" :height 1.2 :weight 'normal)

;; definir valores de la fuente fija (mono)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 110 :weight 'light)

;;;;;;; pulsar ;;;;;;;

;; Resalta momentaneamente la línea donde esta el point
;; después ejecutar las funciones en `pulsar-pulse-functions'
;; color de resaltado
(setq pulsar-face 'pulsar-yellow)
;; activar resaltado en todas las funciones que afectan a la ventana activa
(setq pulsar-delay 0.05)
;; atajo para relsatar linea actual
(global-set-key (kbd "C-c p") 'pulsar-pulse-line)
;; se activa globalmente el modo
(pulsar-global-mode 1)
;; se agrega el hacer click a las funciones de pulsar
;; (add-to-list 'pulsar-pulse-functions 'mouse-set-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; FUNCIONES PERZONALIZADAS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Otros paquetes relevantes que he probado:
;; - corfu-doc (ventanitas con la documentación, corfu tiene la variable
;; `corfu-info-documentation' que activa esta función)
;; - rg (implementacion de repgrip en emacs, búsquedas recursivas en contenidos de archivos)
;; - openwith (permite abrir archivos con apliación externa preferida)
;; - org-krita (insertar archivos de krita en org)
;; - citar (gestor de referencias)
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
;; - org-variable-pitch (mezclar fuente variable y fija en documentos de org)

;; Abrir o agregar notas literarias. Usa org-roam de backend
;; y depende de citar. Remplaza la función `citar-open-note-function'.
;; basado en https://jethrokuan.github.io/org-roam-guide/
;; (defun b3m3bi/org-roam-edit-cite-node (key entry)
;;   "Si la nota de KEY existe la abre con `find-file' y si
;; no existe crea un nodo de tipo reference usando `org-roam-capture-' llenando
;; un template con la información en ENTRY"
;;   ;; se obtiene el nombre de la nota y se revisa si existe
;;   ;; el archivo (que ningún elemento de VARLIST sea nil)
;;     (if-let
;; 	((file (citar-file--get-note-filename key
;; 					      citar-notes-paths
;; 					      citar-file-note-extensions))
;; 	 (file-exists (file-exists-p file)))
;; 	;; si se obtuvo el nombre del archivo y si existe se abre
;; 	(find-file file)
;;       ;; si no existe el archivo se crea un nodo nuevo
;;       (let
;; 	  ;; se define el título de la nota
;; 	  ((title (citar--format-entry-no-widths entry "Notes on ${author editor}, ${title}")))
;; 	;; se crea nodo
;; 	(org-roam-capture-
;;        :templates '(("r" "reference" plain "%?"
;; 		     :if-new (file+head "reference/${citekey}.org"
;; 					":PROPERTIES:
;; :ROAM_REFS: @${citekey}
;; :END:
;; #+title: ${title}")
;; 		     :unnarrowed t
;; 		     :empty-lines-before 1))
;;        :info (list :citekey key)
;;        :node (org-roam-node-create :title title)
;;        :props '(:finalize find-file)))))

;; función para activar eglot en `org-edit-special tomada de
;; https://github.com/joaotavora/eglot/issues/216#issuecomment-1052931508
;; (defun mb/org-babel-edit:python ()
;;   "Edit python src block with lsp support by tangling the block and
;; then setting the org-edit-special buffer-file-name to the
;; absolute path. Finally load eglot."
;;   (interactive)

;; ;; org-babel-get-src-block-info returns lang, code_src, and header
;; ;; params; Use nth 2 to get the params and then retrieve the :tangle
;; ;; to get the filename
;;   (setq mb/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))

;;   ;; tangle the src block at point 
;;   (org-babel-tangle '(4))
;;   (org-edit-special)

;;   ;; Now we should be in the special edit buffer with python-mode. Set
;;   ;; the buffer-file-name to the tangled file so that pylsp and
;;   ;; plugins can see an actual file.
;;   (setq-local buffer-file-name mb/tangled-file-name)
;;   (eglot-ensure)
;;   )

;;;;;; embark ;;;;;;

;; (global-set-key (kbd "C-.") 'embark-act)
;; (global-set-key (kbd "M-.") 'embark-dwim)
;; (global-set-key (kbd "C-h b") 'embark-bindings)

;;;;;; winner-mode ;;;;;
;; historial de ordenamiento de las ventanas (páneles)
;; regresar a config previa con C-c {flechas}
;; (winner-mode 1)

;; función para buscar una palabra en un archivo en mi zettel (depende de consult)
;; (defun b3m3bi/org-roam-consult-rg ()
;;   "Consult ripgrep on `org-roam-directory."
;;   (interactive)
;;   (consult-ripgrep org-roam-directory))
;; (global-set-key (kbd "C-c n r") 'b3m3bi/org-roam-consult-rg)

;; Abrir init file
;; (defun b3m3bi/open-init-file ()
;;   "Open init file."
;;   (interactive)
;;   (find-file user-init-file))
;; (global-set-key (kbd "<XF86Favorites>") 'b3m3bi/open-init-file)
;; 
;; ;; Abrir gtd file
;; (defun b3m3bi/open-gtd-file ()
;;   "Open gtd file."
;;   (interactive)
;;   (find-file (concat org-directory "/GTD/gtd.org")))
;; (global-set-key (kbd "C-<XF86Favorites>") 'b3m3bi/open-gtd-file)
;; 
;; ;; Abrir inbox file
;; (defun b3m3bi/open-inbox-file ()
;;   "Open gtd file."
;;   (interactive)
;;   (find-file (concat org-directory "/GTD/inbox.org")))
;; (global-set-key (kbd "M-<XF86Favorites>") 'b3m3bi/open-inbox-file)

;; si se inicia una búsqueda (C-s) en dired sobre el nombre de un archivo
;; solo búsqua en el nombre de los archivos


;; ;; procesador "follow" para abrir archivos o doi de citas (modificado de oc-basic.el)
;; (defun chafa-open (datum _)
;;   (let* ((key
;; 	  (if (eq 'citation-reference (org-element-type datum))
;; 	      (org-element-property :key datum)
;; 	    (pcase (org-cite-get-references datum t)
;; 	      (`(,key) key)
;; 	      (keys
;; 	       (or (completing-read "Select citation key: " keys nil t)
;; 		   (user-error "Aborted"))))))
;; 	 (action
;; 	  (completing-read "Select action: " '("Open file" "Open doi"))))
;;     (cond
;;      ((string= action "Open file") 
;;       (let ((path (org-cite-basic--get-field 'file key)))
;; 	(if path
;; 	    (org-open-file path)
;; 	  (user-error "No associated file"))))
;;      ((string= action "Open doi")
;;       (let ((link (concat "https://doi.org/" (org-cite-basic--get-field 'doi key))))
;; 	(if link
;; 	    (org-open-link-from-string link)
;; 	  (user-error "No associated doi"))))
;;      (t (message "Algo pasó mal :(")))))
;; ;; se registra el procesador (ver cómo funciona org-cite-register-proccessor en oc.el)
;; (org-cite-register-processor 'chafa
;;   :follow 'chafa-open)
;; ;; se asigna el procesador
;; (setq org-cite-follow-processor 'chafa)

;;;;; denote ;;;;;
(setq denote-directory (expand-file-name "~/Zettel"))
(setq denote-file-type 'org)
(setq denote-known-keywords '("ciencia" "matematicas" "productividad" "computacion"))
(global-set-key (kbd "C-c n n") #'denote-subdirectory)
(global-set-key (kbd "C-c n r") #'denote-rename-file)
(global-set-key (kbd "C-c n C-r") #'denote-rename-file-using-front-matter)
;; (global-set-key (kbd "C-c n f") #'denote-open-or-create)
;; (global-set-key (kbd "C-c n i") #'denote-link-or-create)
(global-set-key (kbd "C-c n b") #'denote-link-backlinks)
(setq denote-excluded-directories-regexp "img")

(add-hook 'dired-mode-hook #'denote-dired-mode)

(setq xref-search-program 'ripgrep)

;; agregar paquete para formatear completing-read
(add-to-list 'load-path (expand-file-name "denote-completing-format" "~/.emacs.d/b3m3bi-packages/"))
(require 'denote-completing-format)
(global-set-key (kbd "C-c n f") #'denote-completing-format-open-or-create)
(global-set-key (kbd "C-c n i") #'denote-completing-format-link-or-create)
(setq denote-completing-format-subdir-width 20)
(setq denote-completing-format-keywords-width 25)
(setq denote-completing-format-create-function 'denote-subdirectory)
 
;;;;; citar ;;;;;
(setq citar-bibliography org-cite-global-bibliography)
(setq org-cite-insert-processor 'citar)
(setq org-cite-follow-processor 'citar)
(setq org-cite-activate-processor 'citar)
(setq citar-open-prompt t)
(require 'citar-file)
(add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))


(setq resize-mini-windows 'grow-only)

;;;;;; org-variable-pitch ;;;;;;

(require 'org-variable-pitch)
;; definir fuente que usa org-variable-pitch
(set-face-attribute 'org-variable-pitch-fixed-face nil :family "JetBrains Mono" :height 110 :weight 'light)
(add-to-list 'org-variable-pitch-fixed-faces 'font-lock-comment-face)
(setq org-variable-pitch-fontify-headline-prefix nil)

(add-to-list 'load-path (expand-file-name "org-variable-pitch-custom" "~/.emacs.d/b3m3bi-packages/"))
(require 'org-variable-pitch-custom)
(add-to-list 'org-variable-pitch-custom-group 'org-document-title)
(add-to-list 'org-variable-pitch-custom-group 'org-document-info)

;;;;; almost-mono-themes ;;;;;

;; (add-to-list 'load-path (expand-file-name "almost-mono-themes" "~/.emacs.d/manual-packages"))
;; (add-to-list 'custom-theme-load-path (expand-file-name "almost-mono-themes" "~/.emacs.d/manual-packages"))
;; (mapc #'disable-theme custom-enabled-themes)
;; ;; (load-theme 'almost-mono-white :no-confirm)
;; (load-theme 'modus-vivendi :no-confirm)

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
(add-hook 'python-mode-hook #'linum-mode)

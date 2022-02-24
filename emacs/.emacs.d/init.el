;;;;;;;;;; CONFIGURACIÓN GENERAL ;;;;;;;;;;

;; se quita el mensaje de inicio
(setq inhibit-startup-message t)

;; se mandan las ediciones de customize a otro archivo y se cargan
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;;;;;;;;; PAQUETES ;;;;;;;;;;

;; 
;; se carga e inicializa straight.el (un administrador de paquetes)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

;; usar use-package (para tener más ordenadito este archivo)
(straight-use-package 'use-package)

;; usar y configurar helm
(use-package helm
	     :straight t
	     :init
	     (helm-mode 1)
	     :bind (("M-x" . helm-M-x)))

;; usar y configurar org-roam
(use-package org-roam
	     :straight t
	     :init
	     (setq org-roam-v2-ack t)
	     (setq org-roam-directory "~/Zettel")
	     (org-roam-db-autosync-mode)
	     :bind (("C-c n f" . org-roam-node-find)
		    ("C-c n i" . org-roam-node-insert)
		    ("C-c n b" . org-roam-buffer-toggle))
	     :config
	     (org-roam-setup))

;(use-package citar
;  :straight t
;  :config
;  (setq citar-notes-paths '("~/Prueba/notas.org")))

;; este paquete permite buscar y administrar bibliografías
;; (e.g., abrirlas en el navegador, insertar la cita)
;; bibtex usando helm de front-end
(use-package helm-bibtex
  :straight t
  :init
  ;; archivos de bibliografía en los que va a buscar
  (setq bibtex-completion-bibliography '("~/Zettel/bibliography/ref-all.bib")
	;; usar el campo de file para encontrar la ubicación del pdf
	bibtex-completion-pdf-field "file"
	;; definir directorio/archivo donde van a guardarse las notas de los artículos
	bibtex-completion-notes-path "~/Zettel/bibliography/notes.org")
  ;; definir la aplicación para abrir el pdf
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "okular" nil 0 nil fpath))))

(org-cite-register-processor 'my-bibtex-org-cite-follow
  :follow (lambda (_ _) (helm-bibtex)))
(setq org-cite-follow-processor 'my-bibtex-org-cite-follow)


(use-package visual-fill-column
	     :straight t
	     :init
	     (add-hook 'org-mode-hook 'visual-line-mode)
	     (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
	     (setq visual-fill-column-center-text t))


(use-package org-roam-ui
	     :straight 
	     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
	     :after org-roam
	     :config
	     (setq org-roam-ui-sync-theme t
		   org-roam-ui-follow t
		   org-roam-ui-update-on-save t
		   org-roam-ui-open-on-start t))

;; tema
(use-package modus-themes
  :straight
  (:host github :repo "protesilaos/modus-themes" :branch "main" :files ("*.el"))
  :init
  (setq modus-themes-org-blocks 'gray-background)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;; soporte para usar el kernel de jupyter en org-mode
(use-package ob-ipython
  :straight t)

;; variables de org-mode
(setq org-babel-load-languages
      '((python . t)
	(ipython . t)
	(R . t)))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; cambiar python a python3
(setq python-shell-interpreter "python3")

;; se activan las imágenes en línea
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; se esconden los marcadores de enfasis
;setq org-hide-emphasis-markers t)

;; se permite cambiar el tamaño de las imágenes en el preview
(setq org-image-actual-width nil)

;; se quita toolbar, menubar y scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; agregar números a las lineas
;(global-display-line-numbers-mode 1)

;; en lugar de sonar hace flashes
(setq visible-bell 1)

;; mostrar imágenes al cargar un archivo .org
(add-hook 'org-mode-hook 'org-toggle-inline-images)

;; desactivar la identación automática en los bloques SRC de org-mode
(setq org-src-preserve-identation nil
      org-edit-src-content-indentation 0)

;; seguir un symlink sin tener que confirmar
(setq vc-follow-symlinks t)

;; cambiar tamaño de latex preview
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.25))

;; abrir con aplicaciones que si quiero
(setq org-file-apps
      '((auto-mode . emacs)
;;        ("\\.x?html?\\'" . "firefox %s")
        ("\\.pdf\\'" . "okular \"%s\"")
;;        ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
;;       ("\\.pdf.xoj" . "xournal %s")))
))

;; permite mezclar fuentes variables (proporcionales) y mono (fijas)
(use-package mixed-pitch
  :straight t)

;; determinar cual es la fuente variable (proporcional)
(set-face-attribute 'variable-pitch nil :family "Roboto" :height 120 :weight 'light)

;; autocompletado
(use-package company
  :straight t)


;; Este paquete debe permitir insertar imagenes con drag and drop.
;; ENTENDER COMO FUNCIONA ESTO!!!!
;; Insertar imágenes es muy facil, por lo que no creo necesitarlo...
;(use-package org-download
;  :straight t
;  :init
;  (add-hook 'dired-mode-hook 'org-download-enable))

;; resalta los bloques latex en org
(setq org-highlight-latex-and-related '(latex))
(set-face-attribute 'org-latex-and-related nil :foreground "#854b6b")


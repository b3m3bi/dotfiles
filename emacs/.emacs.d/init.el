;; quitar mensaje del inicio
(setq inhibit-startup-message t)

;; se mandan las ediciones de customize a otro archivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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

(use-package org-ref
	     :straight t
	     :config
	     (setq reftex-default-bibliography '("~/Zettel/bibliography/references.bib"))
	     (setq org-ref-bibliography-notes "~/Zettel/bibliography/notes.org"
		   org-ref-default-bibliography '("~/Zettel/bibliography/references.bib")
		   org-ref-pdf-directory "~/Zettel/bibliography/bibtex-pdfs/"))


(use-package org-roam-bibtex
	     :straight t
	     :after org-roam
	     :config
	     (org-roam-bibtex-mode))

(use-package visual-fill-column
	     :straight t
	     :init
	     (add-hook 'org-mode-hook 'visual-line-mode)
	     (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
	     ;(setq visual-fill-column-center-text t)
	     )


(use-package org-roam-ui
	     :straight 
	     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
	     :after org-roam
	     :config
	     (setq org-roam-ui-sync-theme t
		   org-roam-ui-follow t
		   org-roam-ui-update-on-save t
		   org-roam-ui-open-on-start t))

(use-package monokai-theme
  :straight
  (:host github :repo "oneKelvinSmith/monokai-emacs" :branch "master" :files ("*.el")))

(use-package modus-themes
  :straight
  (:host github :repo "protesilaos/modus-themes" :branch "main" :files ("*.el"))
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))


(use-package ob-ipython
  :straight t)

(setq org-babel-load-languages
      '((python . t)
	(ipython . t)
	(R . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

(setq org-babel-python-command "python3")
(setq python-shell-interpreter "python3")

;; se activan las imágenes en línea
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; se esconden los marcadores de enfasis
;(setq org-hide-emphasis-markers t)

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


;; cambiar fuente en org-mode
;(defun my-buffer-face-mode-variable ()
;  "Utilizar una fuente con longitud variable (proporcional)"
;  (interactive)
;  (setq buffer-face-mode-face '(:family "Roboto Mono" :weight light :height 120 ))
;  (buffer-face-mode))

; (add-hook 'org-mode-hook 'my-buffer-face-mode-variable)

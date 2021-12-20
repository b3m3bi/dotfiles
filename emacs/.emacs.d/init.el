;; se mandan las ediciones de customize a otro archivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; se cambia el tema
(load-theme 'leuven)

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

; usar y configurar org-roam
(use-package org-roam
	     :straight t
	     :init
	     (setq org-roam-v2-ack t)
	     (setq org-roam-directory "~/org-roam")
	     (org-roam-db-autosync-mode)
	     :bind (("C-c n f" . org-roam-node-find)
		    ("C-c n i" . org-roam-node-insert)
		    ("C-c n b" . org-roam-node-insert))
	     :config
	     (org-roam-setup))

(use-package org-ref
	     :straight t
	     :config
	     (setq reftex-default-bibliography '("~/org-roam/bibliography/references.bib"))
	     (setq org-ref-bibliography-notes "~/org-roam/bibliography/notes.org"
		   org-ref-default-bibliography '("~/org-roam/bibliography/references.bib")
		   org-ref-pdf-directory "~/org-roam/bibliography/bibtex-pdfs/"))


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
; se activan las imágenes en línea
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

; se esconden los marcadores de enfasis
(setq org-hide-emphasis-markers t)

; se permite cambiar el tamaño de las imágenes en el preview
(setq org-image-actual-width nil)

; se quita toolbar y menubar
(menu-bar-mode -1)
(tool-bar-mode -1)

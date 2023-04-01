;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers '(
                                       ;; ----------------------------------------------------------------
                                       ;; Example of useful layers you may want to use right away.
                                       ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
                                       ;; <M-m f e R> (Emacs style) to install them.
                                       ;; ----------------------------------------------------------------
                                       (org :variables
                                            org-want-todo-bindings t
                                            org-startup-indented t
                                            org-todo-keywords '((sequence "TODO(t!)" "MARK(m!)" "|" "DONE(d!)" "QUIT(q!)"))
                                            org-agenda-window-setup "current-window"
                                            org-agenda-restore-windows-after-quit t
                                            org-agenda-start-on-weekday 0
                                            org-agenda-entry-text-maxlines 1
                                            org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 4 :fileskip0 t)
                                            org-default-priority ?C
                                            ;; babel
                                            org-babel-python-command "python3"
                                            org-export-babel-evaluate nil
                                            org-export-with-toc nil
                                            org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                                                    "xelatex -shell-escape -interaction nonstopmode %f")
                                            org-latex-listings 'minted
                                            org-html-validation-link nil
                                            org-html-doctype "html5"
                                            org-outline-path-complete-in-steps nil
                                            org-timer-default-timer 45 ; default clock countdown
                                            org-log-into-drawer "LOGBOOK" ; default log org-drawer
                                            org-babel-sh-command "bash"
                                            org-plantuml-jar-path (expand-file-name "/opt/plantuml/plantuml.jar")
                                            ;; org roam
                                            org-enable-roam-support t
                                            org-enable-roam-protocol t
                                            org-enable-roam-server t
                                            ;; org-habit
                                            org-habit-show-done-always-green t
                                            org-habit-graph-column 70
                                            org-habit-preceding-days 7
                                            org-habit-following-days 1
                                            ;; org-journal
                                            org-enable-org-journal-support t
                                            org-journal-file-type 'daily
                                            org-journal-enable-encryption nil
                                            org-journal-dir (expand-file-name "~/Wally/Journal/journals")
                                            org-journal-file-format "%Y_%m_%d.org"
                                            org-journal-find-file 'find-file
                                            org-journal-start-on-weekday 7
                                            org-journal-date-format "%A, %B %d %Y"
                                            org-journal-time-format "*%H.%M*\n\n"
                                            org-journal-time-prefix "\n** "

                                            ;; misc
                                            org-id-link-to-org-use-id t
                                            org-enable-hugo-support t
                                            )
                                       auto-completion
                                       better-defaults
                                       (chinese :variables
                                                ;; chinese-default-input-method 'wubi
                                                pangu-spacing-real-insert-separtor nil
                                                chinese-enable-youdao-dict t)
                                       (c-c++ :variables
                                              tab-width 4
                                              c-c++-default-mode-for-headers 'c++-mode
                                              c-c++-enable-clang-support t
                                              clang-format-style-option "Google"
                                              c-c++-enable-google-newline t
                                              c-c++-enable-google-style t
                                              c-c++-enable-rtags-support t
                                              c-c++-backend 'lsp-clangd
                                              c-basic-offset 4
                                              c-c++-lsp-enable-semantic-highlight 'rainbow
                                              c-c++-enable-clang-format-on-save nil
                                              c-c++-enable-organize-includes-on-save nil
                                              c-c++-enable-auto-newline nil
                                              )
                                       (cmake :variables
                                              cmake-backend 'company-cmake
                                              cmake-enable-cmake-ide-support nil
                                              )
                                       ;; chrome
                                       csv
                                       (deft :variables
                                         deft-recursive t
                                         deft-use-filter-string-for-filename t
                                         deft-default-extension "org"
                                         deft-directory "~/Wally/deft"
                                         )
                                       django
                                       docker
                                       ;; eaf
                                       (elfeed :variables
                                               rmh-elfeed-org-files
                                               (list
                                                "~/Wally/Journal/data/rss.org")
                                               elfeed-enable-web-interface
                                               nil elfeed-search-filter
                                               "@1-weeks-ago +unread"
                                               elfeed-goodies/entry-pane-position
                                               'bottom
                                               elfeed-goodies/entry-pane-size
                                               0.5)
                                       emacs-lisp
                                       epub
                                       finance
                                       ;; extra-langs
                                       git
                                       (go :variables go-use-gometalinter
                                           t go-tab-width 4 gofmt-command "goimports")
                                       graphviz
                                       gtags
                                       helm
                                       (html :variables
                                             web-mode-markup-indent-offset
                                             2 web-mode-css-indent-offset
                                             2
                                             web-mode-code-indent-offset
                                             2 web-mode-indent-style 2)
                                       (java :variables eclim-eclipse-dirs
                                             '("/opt/eclipse") eclim-executable
                                             "~/.p2/pool/plugins/org.eclim_2.8.0/bin/eclim")
                                       (javascript :variables js2-basic-offset
                                                   2 js2-mode-show-parse-errors nil
                                                   js2-mode-show-strict-warnings nil)
                                       (json :variable
                                             json-fmt-on-save t
                                             json-fmt-tool 'web-beautify)
                                       latex
                                       lsp
                                       lua
                                       markdown
                                       multiple-cursors
                                       nginx
                                       (osx :variables osx-command-as       'hyper
                                            osx-option-as        'meta
                                            osx-control-as       'control
                                            osx-function-as      nil
                                            osx-right-command-as 'left
                                            osx-right-option-as  'left
                                            osx-right-control-as 'left
                                            osx-swap-option-and-command nil)
                                       pandoc
                                       ;; pdf-tools
                                       pdf
                                       plantuml
                                       (python :variables
                                               python-backend 'anaconda
                                               python-sort-imports-on-save t
                                               python-enable-yapf-format-on-save t)
                                       racket restclient
                                       search-engine
                                       semantic
                                       (shell :variables
                                              shell-default-height 30
                                              shell-default-position 'bottom)
                                       shell-scripts
                                       spacemacs-defaults
                                       spacemacs-editing
                                       spacemacs-editing-visual
                                       spacemacs-navigation
                                       (spell-checking :variables spell-checking-enable-by-default nil)
                                       sql
                                       syntax-checking
                                       treemacs
                                       yaml
                                       ;; private
                                       hugo
                                       (myleetcode :variables
                                                 leetcode-save-solutions t
                                                 leetcode-prefer-language "c"
                                                 leetcode-prefer-sql "sqlite3"
                                                 leetcode-directory "~/Project/codelet/source/solutions/C"
                                                 )
                                       wally
                                       )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(org-transform-tree-table
                                      org-pdftools
                                      org-noter-pdftools
                                      quelpa-use-package
                                      pretty-hydra
                                      org-ref
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 60

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key ";"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; (setq configuration-layer-elpa-archives '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
  ;;                                           ("org-cn"   . "http://elpa.emacs-china.org/org/")
  ;;                                           ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")
  ;;                                           ("nongnu"   . "https://elpa.nongnu.org/nongnu/")))

  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
          ("org-cn"   . "http://mirrors.ustc.edu.cn/elpa/org/")
          ("gnu-cn"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
          ("nongnu"   . "https://elpa.nongnu.org/nongnu/")))


  (setq dotspacemacs-scratch-buffer-persistent t
        dotspacemacs-activate-smartparens-mode t)

  (setq wally-journal-dir "~/Wally/Journal/"
        wally-note-dir (concat wally-journal-dir "core/")
        wally-refs-dir (concat wally-journal-dir "refs/")
        wally-dice-dir (concat wally-journal-dir "dice/")
        wally-gtd-dir (concat wally-journal-dir "gtd/")
        wally-db-dir (concat wally-journal-dir "dice/")
        wally-data-dir (concat wally-journal-dir "data/")
        wally-note-file (concat wally-gtd-dir "journal.org"))
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; auto-mode-alist
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.qrc\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.xarco\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))


  ;; hook
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")
              ))

  (add-hook 'org-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              ))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              ))

  (add-hook 'kill-buffer-hook 'wally/snap-auto-delete)

  (setq mac-command-modifier 'meta)
  (setq whitespace-line-column 121) ;; limit line length
  (setq whitespace-style '(face lines-tail))

  (setq wally-emacs-server (format "http://localhost:%s"  httpd-port)
        wally-emacs-server-proxy (format "http://%s:%s" (get-ip-address "en0") httpd-port-proxy)
        )

  (setq wally-evil-file-server (format "http://%s:4201" (get-ip-address "en0")))
  (setq wally-evil-file-root "/Volumes/Wally/Media/.party/.private")

  ;; epc
  (defvar wally-habit-epc (epc:start-epc "python3.9" (list (expand-file-name "~/Project/empyc/srv/habit/epcsrv.py"))))

  (eval-after-load "dired-aux"
    '(add-to-list 'dired-compress-file-suffixes
                  '("\\.zip\\'" ".zip" "unzip")))

  ;; smartparens
  (show-smartparens-global-mode 1)
  (smartparens-global-mode t)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "ret")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "spc")
                                              ("* ||\n[i]" "ret"))))
  (sp-local-pair 'org-mode "《" "》")
  (sp-local-pair 'org-mode "（" "）")
  (sp-local-pair 'org-mode "“" "”")
  (sp-local-pair 'org-mode "\[" "\]")
  ;; (sp-local-pair 'org-mode " ~" "~ "
  ;;                :trigger " ~"
  ;;                :wrap "C-)")
  ;; (sp-local-pair 'org-mode "  *" "*  "
  ;;                :trigger " *"
  ;;                :wrap "C-*")
  ;; (sp-local-pair 'org-mode " +" "+ "
  ;;                :trigger " +"
  ;;                :wrap "C-+")
  ;; (sp-local-pair 'org-mode " =" "= "
  ;;                :wrap "C-=")

  (when (string-equal system-type "darwin")
    (setenv "CC" "/usr/local/bin/gcc-11")
    (setenv "CXX" "/usr/local/bin/g++-11")
    (setenv "PATH" (format "%s:%s" "/usr/local/bin" (getenv "PATH")))
    (setenv "PATH" (format "%s:%s" "/Applications/Google Chrome.app/Contents/MacOS:$PATH" (getenv "PATH")))
    )

  ;; variables
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (if (eq system-type 'darwin)
                                       "open"
                                     "firefox"))

  (setq default-buffer-file-coding-system 'utf-8)

  (setenv "GPG_AGENT_INFO" nil)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t
        epa-file-inhibit-auto-save nil)
  (setf epa-pinentry-mode 'loopback)

  (setenv "GTAGSLIBPATH" (expand-file-name "~/Project/.gtags"))

  (setq bookmark-default-file (f-join wally-journal-dir "data/bookmarks"))
  (setq debug-on-error nil)

  (setq projectile-indexing-method 'native)
  (setq search-engine-alist
        '((google :name "Google" :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
          (bing :name "Bing" :url "http://www.bing.com/search?q=%s")
          (github :name "Github" :url "https://github.com/search?ref=simplesearch&q=%s")
          (stack-overflow :name "Stack Overflow" :url "https://stackoverflow.com/search?q=%s")
          (wikipedia :name "Wikipedia" :url "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
          ;; (amazon :name "Amazon" :url "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
          ;; (duck-duck-go :name "Duck Duck Go" :url "https://duckduckgo.com/?q=%s")
          ;; (google-images :name "Google Images" :url "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
          ;; (google-maps :name "Google Maps" :url "http://maps.google.com/maps?q=%s")
          ;; (twitter :name "Twitter" :url "https://twitter.com/search?q=%s")
          ;; (project-gutenberg :name "Project Gutenberg" :url "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
          ;; (youtube :name "YouTube" :url "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
          ;; (spacemacs-issues :name "Spacemacs Issues" :url "https://github.com/syl20bnr/spacemacs/issues?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
          ;; (spacemacs-pullrequests :name "Spacemacs Pull Requests" :url "https://github.com/syl20bnr/spacemacs/pulls?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
          ;; (wolfram-alpha :name "Wolfram Alpha" :url "http://www.wolframalpha.com/input/?i=%s")

          (cmake :name "CMake" :url "https://cmake.org/cmake/help/v3.0/search.html?q=%s&check_keywords=yes&area=default")
          (opencv :name "OpenCV" :url "https://docs.opencv.org/master/search/all_d.html?%s")
          (eudic :name "eudic" :url "https://dict.eudic.net/dicts/en/%s")
          (youdao :name "youdao" :url "https://www.youdao.com/w/%s")
          (mdn :name "MDN" :url "https://developer.mozilla.org/zh-CN/search?q=%s")
          (zhihu :name "zhihu" :url "https://www.zhihu.com/search?&q=%s")
          (douban :name "douban" :url "https://www.douban.com/search?q=%s")
          (cpp :name "cpp" :url "http://www.cplusplus.com/search.do?q=%s")
          (leetcode :name "leetcode" :url "https://leetcode-cn.com/problems/%s/")
          )
        )

  ;; org-protocol begin
  (org-link-set-parameters
   "org-protocol"
   :export (lambda (path desc backend)
             (cond
              ((eq 'html backend)
               (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
              ((or (eq 'hugo backend) (eq 'md backend))
               (format "[%s](org-protocol:%s)" desc path))
              )))

  (org-link-set-parameters
   "org-protocol"
   :export (lambda (path desc backend)
             (cond
              ((eq 'html backend)
               (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
              ((eq 'hugo backend)
               (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
              ((eq 'md backend)
               (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
              )))

  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist
               '("org-id" :protocol "org-id"
                 :function org-id-protocol-goto-org-id))

  (defvar org-protocol-open-file-at-line nil
    "A property list used by `org-protocol-server-visit-hook-fun'.")

  (defun org-protocol-server-visit-hook-fun ()
    "Jump to line number detected by `org-protocol-open-file'.
Transferred from `org-protocol-open-file' via variable `org-protocol-open-file-at-line'."
    (let ((file (plist-get org-protocol-open-file-at-line :file))
          (line (1- (string-to-number (or (plist-get org-protocol-open-file-at-line :line) "1")))))
      (when (file-equal-p file (buffer-file-name))
        (unless (plist-get org-protocol-open-file-at-line :read-only)
          (read-only-mode -1))
        (goto-char (point-min)) ;; should actually already be there...
        (forward-line line))))

  (add-hook 'server-visit-hook #'org-protocol-server-visit-hook-fun)

  (defun org-protocol-open-file (pars)
    "Act as org-protocol-protocol-alist function with PARS as args.
Handle links like: org-protocol://open-file?file=/foo/ss/subdir/foo.js&line=132"
    (let ((file (plist-get pars :file)))
      (setq org-protocol-open-file-at-line pars)
      (and (file-exists-p file)
           file)))

  (eval-after-load 'org-protocol
    (lambda ()
      (add-to-list 'org-protocol-protocol-alist
                   '("open-file"
                     :protocol "open-file"
                     :function org-protocol-open-file))))

  ;; org-protocol end

  ;; mode
  (engine-mode nil)
  (engine-mode t)
  (global-pangu-spacing-mode 1)
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)
  (epa-file-enable)
  (org-super-agenda-mode 1)

  (with-eval-after-load 'org
    (require 'org-tempo)
    )

  ;; serverlets
  (defservlet* dice/:db nil (count)
    (let ((db-file (f-join wally-dice-dir (format "%s.org" db))))
      (if (not count)
          (setq count 1))
      (if (f-exists-p db-file)
          (wally//dice-item db-file count)
        (message "%s does not exist!" db-file))))

  (defservlet* evil/:action/:subdir/:leafdir nil (leaf)
    (let ((item (f-join subdir leafdir))
          (default-directory wally-evil-file-root ))
      (if leaf
          (setq item (f-join item leaf)))
      (cond
       ((s-equals-p action "trash")
        (progn
          (wally/evil-db-degrade-item item)
          (when (f-exists-p item)
            (move-file-to-trash item)
            (message "trash %s" item))))
       ((s-equals-p action "like")
        (wally/evil-db-upgrade-item item)))))

  (defservlet dice/evil nil (path)
    (wally//dice-db-items "7AEA69C2-674E-4D66-8327-09BE16C6B2F5"
                          '(("ximg.org" . 5) ("xsht.org" . 3) ("xflm.org" . 1)) t))

  ;; misc
  (prefer-coding-system 'utf-8)
  (remove-hook 'server-visit-hook #'org-protocol-server-visit-hook-fun)

  ;; action
  (server-start)
  (httpd-start)

  ;; tmp
  (defun wally/org-set-default-line-spacing ()
    (setq-local line-spacing 0.25))
  (add-hook 'org-mode-hook 'wally/org-set-default-line-spacing)


  (defun wally/text-mode-set-line-width ()
    (setq-local fill-column 180))

  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

  (add-hook 'text-mode-hook 'spacemacs/toggle-truncate-lines-off)
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  (add-hook 'text-mode-hook 'wally/text-mode-set-line-width)

  ;; tail
  (display-time-mode 1)
  )

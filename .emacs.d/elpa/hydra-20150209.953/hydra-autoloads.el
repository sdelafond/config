;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (defhydra hydra-create) "hydra" "hydra.el" (21722
;;;;;;  17673 618195 242000))
;;; Generated autoloads from hydra.el

(autoload 'hydra-create "hydra" "\
Create a hydra with a BODY prefix and HEADS with METHOD.
This will result in `global-set-key' statements with the keys
being the concatenation of BODY and each head in HEADS.  HEADS is
an list of (KEY FUNCTION &optional HINT).

After one of the HEADS is called via BODY+KEY, it and the other
HEADS can be called with only KEY (no need for BODY).  This state
is broken once any key binding that is not in HEADS is called.

METHOD is a lambda takes two arguments: a KEY and a COMMAND.
It defaults to `global-set-key'.
When `(keymapp METHOD)`, it becomes:

    (lambda (key command) (define-key METHOD key command))

\(fn BODY HEADS &optional METHOD)" nil t)

(put 'hydra-create 'lisp-indent-function '1)

(make-obsolete 'hydra-create 'defhydra "0.8.0")

(autoload 'defhydra "hydra" "\
Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY, and will set the transient map so that all
following heads can be called though KEY only.

The heads inherit their PLIST from the body and are allowed to
override each key.  The keys recognized are :color and :bind.
:color can be:

- red (default): this head will continue the Hydra state.
- blue: this head will stop the Hydra state.
- amaranth (applies to body only): similar to red, but no binding
except a blue head can stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(put 'defhydra 'lisp-indent-function '2)

;;;***

;;;### (autoloads nil nil ("hydra-examples.el" "hydra-pkg.el") (21722
;;;;;;  17673 798045 865000))

;;;***

(provide 'hydra-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hydra-autoloads.el ends here

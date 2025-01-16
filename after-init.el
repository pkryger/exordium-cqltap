;;; after-init.el --- CQL support                   -*- lexical-binding: t; -*-

;;; Commentary:
;; Cassandra - CQL support with sql-mode and ob-sql

;;; Code:


(require 'sql)
(require 'org)

(defun pk/sql--custom-set-product-feature-factory (feature)
  "Create a custom set function that update the FEATURE for Cassandra in `sql'."
  #'(lambda (sym value)
      (sql-set-product-feature 'cql
                               feature
                               (if (member feature sql-indirect-features)
                                   sym
                                 value))
      (set-default sym value)))
:config
(setf sql-product-alist (assoc-delete-all 'cql sql-product-alist))
(sql-add-product 'cql "Cassandra"
                 :free-software t)

;; CQL keywords from (without ANSI keywords):
;; https://cassandra.apache.org/doc/latest/cassandra/cql/appendices.html#appendix-A
(defvar pk/sql-cql-font-lock-keywords
  `(,(sql-font-lock-keywords-builder 'font-lock-type-face nil
                                     "ascii" "bigint" "counter" "inet" "text" "timeuuid" "tinyint" "uuid" "varint"
                                     "bitstring" "byte" "complex" "enum" "macaddr")
    ,(sql-font-lock-keywords-builder 'font-lock-keyword-face nil
                                     "allow" "apply" "authorize" "batch" "clustering" "columnfamily" "compact"
                                     "counter" "custom" "entries" "filtering" "finalfunc" "frozen" "functions"
                                     "if" "index" "infinity" "initcond" "json" "keys" "keyspace" "keyspaces" "list"
                                     "login" "nan" "nologin" "norecursive" "nosuperuser" "password" "permission"
                                     "permissions" "rename" "replace" "roles" "sfunc" "storage" "stype" "superuser"
                                     "timeuuid" "token" "truncate" "ttl" "tuple" "unlogged" "use" "users" "writetime"
                                     "maxwritetime"))
  "Cassandra CQL keywords used by font-lock.")
(sql-set-product-feature 'cql
                         :font-lock 'pk/sql-cql-font-lock-keywords)

;; C-style comments // and /**/ (the latter is supported by `sql-mode')
(sql-set-product-feature 'cql
                         :syntax-alist '((?/ . ". 124")))

(defcustom pk/sql-cql-program "cqlsh"
  "Command to start cqlsh."
  :type 'file
  :group 'SQL
  :set (pk/sql--custom-set-product-feature-factory :sqli-program))
(defcustom pk/sql-cql-login-params '(user password host port)
  "List of login parameters needed to connect to Cassandra."
  :type 'sql-login-params
  :group 'SQL
  :set (pk/sql--custom-set-product-feature-factory :login-params))
(defcustom pk/sql-cql-options nil
  "List of additional options for `pk/sql-cql-program'."
  :type '(repeat string)
  :group 'SQL
  :set (pk/sql--custom-set-product-feature-factory :sqli-options))

(sql-set-product-feature 'cql
                         :prompt-regexp
                         (rx bol
                             (zero-or-one (any "a-z0-9_") "@")
                             "cqlsh"
                             (zero-or-one ":" (any "a-z") (any "a-z0-9"))
                             ">"))
(sql-set-product-feature 'cql
                         :prompt-length 7)
(sql-set-product-feature 'cql
                         :prompt-cont-regexp "^   \\.\\.\\.")
(sql-set-product-feature 'cql
                         :list-all "DESCRIBE TABLES;")
(sql-set-product-feature 'cql
                         :list-table "DESCRIBE %s;")

(defun pk/sql-comint-cql (product params &optional buf-name)
  "Create comint buffer and connect to Cassandra.

PRODUCT is the SQL product.  PARAMS is a list of strings which are
passed as command line arguments.  BUF-NAME is the name of the new
buffer.  If nil, a name is chosen for it."
  (let ((params
         (append
          (if (not (string= "" sql-user))
              (list "-u" sql-user))
          (if (not (string= "" sql-password))
              (list "-p" sql-password))
          params
          (when (not (string= "" sql-server))
            (list sql-server)
            (if (not (= 0 sql-port))
                (list (number-to-string sql-port)))))))
    (sql-comint product params buf-name)))
(sql-set-product-feature 'cql
                         :sqli-comint-func 'pk/sql-comint-cql)

(defun pk/sql-cql (&optional buffer)
  "Run cqlsh by Cassandra as an inferior process in the BUFFER."
  (interactive "P")
  (sql-product-interactive 'cql buffer))

(defun pk/sql-mode-with-cql-product ()
  "Enter `sql-mode' with the `cql' product set."
  (setq-local sql-product 'cql)
  (sql-mode))

(add-to-list 'auto-mode-alist '("\\.cql\\'" . pk/sql-mode-with-cql-product))


;; BEGIN: a hacky way to get font-lock per engine in org-mode
(defvar pk/sql--ob-fontify-engine nil)
(defvar pk/sql--ob-fontify-product-orig nil)

(defun pk/sql--ob-fontify-init-engine (limit)
  "Extract `:engine' from an `org-mode' block SRC block.

This is intended to be used as an advise for
`org-fontify-meta-lines-and-blocks'.  LIMIT has the same meaning
as the advised function."
  (when org-src-fontify-natively
    (unless pk/sql--ob-fontify-product-orig
      (setq pk/sql--ob-fontify-product-orig sql-product))
    (save-excursion
      (setq pk/sql--ob-fontify-engine
            ;; TODO: use (org-babel-get-src-block-info t), see
            ;; https://stackoverflow.com/a/66911315/519827
            (let ((case-fold-search t))
              (when (re-search-forward
                     (rx bol
                         (zero-or-more (any " \t")) "#+begin_src"
                         (one-or-more (any " \t")) "sql"
                         (zero-or-more (one-or-more (any " \t")) (zero-or-more any))
                         (one-or-more (any " \t")) ":engine"
                         (one-or-more (any " \t")) (group (one-or-more (any "a-zA-Z")))
                         (zero-or-more (one-or-more (any " \t")) (zero-or-more any))
                         eol)
                     limit t)
                (match-string-no-properties 1)))))))

(defun pk/sql--ob-fontify-set-product (&rest _)
  "Set `sql-product' according to `:engine'.
As extracted by `pk/sql--ob-fontify-init-engine'.

This is intended to be used as an advise for `org-font-lock-ensure'."
  (when (and org-src-fontify-natively
             (eq major-mode 'sql-mode))
    (sql-set-product (if pk/sql--ob-fontify-engine
                         (if-let* ((product (intern pk/sql--ob-fontify-engine))
                                   ((assoc product sql-product-alist)))
                             product
                           'ansi)
                       'ansi)))
  (setq pk/sql--ob-fontify-engine nil))

(defun pk/sql--ob-fontify-reset-product (&rest _)
  "Reset `sql-product' back to the original value.

This is intended to be used as an advise for
`org-fontify-meta-lines-and-blocks'."
  (when pk/sql--ob-fontify-product-orig
    (sql-set-product pk/sql--ob-fontify-product-orig)
    (setq pk/sql--ob-fontify-product-orig nil)))

(advice-add 'org-fontify-meta-lines-and-blocks :before #'pk/sql--ob-fontify-init-engine)
;;(advice-add 'org-html-fontify-code :before #'pk/sql--ob-fontify-init-engine)
(advice-add 'org-font-lock-ensure :before #'pk/sql--ob-fontify-set-product)
(advice-add 'org-fontify-meta-lines-and-blocks :after #'pk/sql--ob-fontify-reset-product)
;;(advice-add 'org-html-fontify-code :after #'pk/sql--ob-fontify-reset-product)
;; END: a hacky way to get font-lock per engine in org-mode


(provide 'after-init)

;;; after-init.el ends here

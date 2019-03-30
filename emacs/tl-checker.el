(flycheck-define-checker tl-tlcheck
  "A TypedLua syntax checker using tlcheck.

TODO modifiy url : See URL `https://github.com/mpeterv/luacheck'."
  :command ("tlcheck"
            "-sw"
            ;;(option-list "--std" flycheck-luacheck-standards)
            ;;(config-file "--config" flycheck-luacheckrc)
            "--filename" source-original
            ;; Read from standard input
            "stdin")
  :standard-input t
  :error-patterns
  ((warning line-start
            (optional (file-name))
            ":" line ":" column
            ": warning,"
            (message) line-end)
   (error line-start
          (optional (file-name))
          ":" line ":" column
          ;; `luacheck' before 0.11.0 did not output codes for errors, hence
          ;; the ID is optional here
          ": syntax error,"
          (message) line-end)
   (error line-start
          (optional (file-name))
          ":" line ":" column
          ;; `luacheck' before 0.11.0 did not output codes for errors, hence
          ;; the ID is optional here
          ": type error,"
          (message) line-end))
  :modes tl-mode)

(add-to-list 'flycheck-checkers 'tl-tlcheck)

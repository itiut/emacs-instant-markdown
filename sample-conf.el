(add-hook 'after-save-hook
          (lambda ()
            (when instant-markdown:server-proc
              (instant-markdown:refresh))))

#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
read -r -d '\0' sleep_forever <<END
(when swank:*communication-style*
  (format swank/backend:*log-output* ";; Sleeping forever...~%")
  (loop (sleep 3600)))
\0
END
exec ccl \
	--eval '(require :asdf)' \
	--eval '(ql:quickload :asdf)' \
	--eval '(asdf:load-system :swank)' \
	--eval '(load #p"load.lisp")' \
	--eval '(swank:create-server :dont-close t)' \
	--eval "$sleep_forever"

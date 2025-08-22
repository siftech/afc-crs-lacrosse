(in-package :common-lisp-user)

(defpackage :musliner
  (:use :common-lisp)
  #+allegro
  (:import-from :excl :compile-file-if-needed :source-file
                :run-shell-command :octets-to-string)

  (:export #:dbug
           #:debug-on #:debug-off
           #:when-dbug
           #:if-dbug
           #:unless-dbug
           #:dbug-once
           #:debug-list-value
           #:*debug-list* #:*output-list* #:*program-name* #:*output-file*
           #:*debug-list-values*
           #:*debug-prefix-function*
           #:flag-debug-prefix
           #:positive #:negative #:beep
           #:sh #:ls #:cd
           #:while
           #:hostname
           #:for #:rank-and-choose #:setappend
           #:getassoc #:setassoc #:setlassoc #:addlassoc #:incfassoc
           #:band-rank-and-choose
           #:permute #:choose
           #:memoize #:defun-memo
           #:strcat
           #:set-equal
           #:sethash
           #:setdelete
           #:*=
           #:ensure-list-if-atom
           #:singleton-p
           #:file-exists-p
           #:factorial #:unique-p #:dupe-choose
           #:stringify #:pl #:setvar #:null-string #:dottify-alist #:trim-suffix #:trim-prefix
           #:map-stride #:map-even #:map-odd #:map-pairs #:map-by-twos
           #:delistify #:flatten-one-level #:docfun #:docvar
           #:find-all
           #:any #:my-every #:my-mapcar #:sum-mapcar #:variable-p
           #:ls #:pwd #:cd #:sh
           #:my-remove-if #:my-remove-if-not #:average
           #:optimization-boilerplate
           #:define-restricted-var
           #:get-command-line-argument
           #:tstamp

           ;; named objects
           #:named-object
           #:init-objects
           #:find-object
           #:nuke-object
           #:name

           ;; randoms
           #:reset-randoms
           #:get-random-state
           #:generate-randoms

           ;; stochastic streams
           #:stochastic-stream
           #:probability
           #:value
           #:lower-bound
           #:upper-bound
           #:discrete-uniform-distribution
           #:uniform-distribution
           #:conditional-probability
           #:discrete-distribution
           #:get-sample
           #:s-sampling
           #:normal-distribution
           #:positive-normal-distribution
           #:exponentially-distributed
           #:distribution-p
           #:random-set
           #:random-subset
           #:random-choice
           #:dont-error

           ;#:profile
           ))

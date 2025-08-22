

;;; -------------------------------------------------------------------------
;;; if you have a list of agents and a list of items that you want to assign as
;;; evenly as possible to the agents, by calling the function fn with the agent
;;; and his list of items (and optional rest-args), this will do it for you.

(defun spread-across (agents items fn &key (min-batch-size 1) &rest rest-args)
  (let* ((num-agents (length agents))
         (num-items (length items))
         (mod (mod num-items num-agents))
         (items-per-agent (max min-batch-size (floor num-items num-agents)))   ;; each will get one more than max(min-batch-size,floor)...until run out of the mod
         this-item-count
	)
  (dbug :top "Spreading ~A items across ~A agents" num-items num-agents)
  (when (null items)
    (return-from spread-across nil))
  (musliner:while items
        (setf this-item-count (+ item-per-agent (if (> mod 0) 1 0)))
	(decf mod)
	(apply fn (pop agents) (subseq items 0 (min this-item-count (length items))) rest-args)
	(setf items (subseq items (min this-item-count (length items)))))
))

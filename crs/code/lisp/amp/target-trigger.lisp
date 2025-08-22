(in-package :fuzzbomb)

#-(or ccl allegro sbcl)
(error "Have not configured class-direct-superclass for this implementation")

;;;   use target-trees slot in amp
;;;(defvar *history-tree-roots* nil
;;;  "The root of the tree keeping track of the task run and the outputs
;;;  they produced.")

(defmacro tree-walk ((first-node child-fn node-var) &rest loop-clauses)
  "Walk a tree and do something on each node.

Provide the first node and a function for generating a list of
children from a given node. Provide a symbol to use as the current
node variable and use that to finish the loop.

NOTE: Should be depth-first on ccl. Traversal order depends on union
implementation."
  (with-gensyms (open-list)
    `(loop for ,open-list = (list ,first-node)
             then (union (rest ,open-list)
                         (,child-fn ,node-var))
           for ,node-var = (first ,open-list)
           while ,node-var
           ,@loop-clauses)))

(defclass hist-tree-node ()
  ((children :initform nil :accessor children)
   (parent :initform nil :initarg :parent :accessor parent)
   (started :initform (get-internal-real-time) :initarg :started :accessor started)
   (finished :initform nil :initarg :finished :accessor finished)
  ))

(defclass hist-tree-target-node (hist-tree-node)
  ((target-msg :initform nil :initarg :target-msg  :accessor target-msg)
   (target :initform nil :initarg :target  :accessor target)))

(defclass hist-tree-task-node (hist-tree-node)
  ((is-summarizer :initform nil :initarg :is-summarizer :accessor is-summarizer)))

(defclass target-triggered (hist-tree-task-node)
  ((target :initform nil :initarg :target :accessor target))
  (:documentation
   "A mixin class indicating the class should be checked to see if it
   applies to new targets."))

(defmethod add-new-target ((task hist-tree-task-node) target-spec)
  (dbug :top "Adding new target ~s" target-spec)
  (unless (typep task 'named-object) (error "Tasks must be named targets"))
  ;; Send the target message back through the process-new-target-msg
  ;; defined in amp.lisp for now.
;;;  (process-new-target-msg
;;;   (list (list :target
               ;;;(list
;;;                     (cons (list :source (name task))
;;;                           (cons (list :type :new-target)
;;;                                 target-msg)))))))
  (setassoc :source (name task) target-spec)
  (setassoc :type :new-target target-spec)
  (setassoc :extra-info (extra-info (target task)) target-spec)
  (dbug :top "target-msg is now: ~s" target-spec)
  (process-new-target-msg (list (list :target (list target-spec)))))

(defmethod task-applies-to-target-p (task-class-name target-node)
  "Method for defining which targets a given task should be run
on. When this method returns T for a class, an instance of that task
is created to run on the given target.  The default (this method) is nil."
  (declare (ignore task-class-name target-node))
  nil)

(defun class-direct-superclasses (class)
  #-(or ccl allegro sbcl)
  (error "Need class-direct-superclasses for this lisp.")
  (#+ccl ccl:class-direct-superclasses
   #+allegro mop:class-direct-superclasses
   #+sbcl sb-mop:class-direct-superclasses
   class))

(defun class-direct-subclasses (class)
  #-(or ccl allegro sbcl)
  (error "Need class-direct-subclasses for this lisp.")
  (#+ccl ccl:class-direct-subclasses
   #+allegro mop:class-direct-subclasses
   #+sbcl sb-mop:class-direct-subclasses
   class))


(defun task-subclass-p (class)
  "Is the class a subclass of the task class?"
  (tree-walk (class class-direct-superclasses
                    current-class)
             if (eq (find-class 'task) current-class) return t))

(defun target-triggered-tasks ()
  "Get a list of all classes which are subclasses of target-triggered
and task"
  (remove-duplicates
   (tree-walk ((find-class 'target-triggered) class-direct-subclasses current-class)
              when (task-subclass-p current-class)
              collect current-class)))

(memoize 'target-triggered-tasks)       ;; assume no dynamic task type defns

(defmethod task-summarizes-tree-p ((root-node hist-tree-target-node)
                                   (task-node hist-tree-task-node))
  "Does the task summarize the tree?

A task summarizes a tree if the task's parent is the root of the
tree (task was spawned by the original target) and the task is a
summarizer task."
  (and (is-summarizer task-node)
       (eq root-node (parent task-node))))

(defun task-tree-completed-p (root-node &key (ignore-summarizers nil))
  "Are all tasks in the given tree completed?

Walks the tree and checks if all tasks are completed. Optionally
ignore summarizer tasks that summarize the given tree."
  (flet ((ignore-task-p (x)
           (and ignore-summarizers
                (task-summarizes-tree-p root-node x))))
    (tree-walk (root-node children cur-node)
               if (and (typep cur-node 'hist-tree-task-node)
                       (not (ignore-task-p cur-node))
                       (not (eq :complete (status cur-node))))
               ;; Found incompleted task. Short circuit.
               return nil

               finally (return t))))

(defmethod summarizer-blocked-p ((node hist-tree-task-node))
  (when (is-summarizer node)
    (not (task-tree-completed-p (parent node) :ignore-summarizers t))))

;;(defmethod executable-target-p ((tn hist-tree-target-node))
;;  (executable-target-p (target tn)))

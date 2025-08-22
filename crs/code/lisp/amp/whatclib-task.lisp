(cl:in-package :fuzzbomb)

(defclass whatclib-task (docker-task-new)
  ()
  (:default-initargs
   :image "whatclib"
   :results ""
  ))

(defmethod initialize-instance :after ((task whatclib-task) &key)
  (dbug :docker-task "initialize-instance :after ((task whatclib-task): ~s" task)
  (setf (output-pathname task)
          (or (output-pathname task)
              (strcat (namestring (path (target task))) "-output/")))
  (ensure-directories-exist (output-pathname task))
  (let ((path (strcat (output-pathname task) "library-address-map.json")))
    (setf (output-pathname task) path)
    (setf (cmd task) (format nil "/realuser.sh /bin/bash --login -c 'cd /neo-fuzz/code/whatclib; ./target/release/whatclib -v identify --limit -f json -o ~A -d ./prebuilt/signatures ~A'"
                             (namestring path)
                             (namestring (path (target task))))))
  (dbug :top "at end of whatclib-task's initialize-instance:after"))

(defmethod post-exec ((task whatclib-task))
  (dbug :top "at whatclib-post-fn")
  (dbug :top "~A~%" (output-pathname task))
  (setattr :lib-addr-map (output-pathname task) (target task))
  (dbug :top "params: ~s" (params (target task)))
  (add-new-target task (params (target task))))

(defmethod task-applies-to-target-p ((task-class-name (eql 'whatclib-task)) target-node)
  (and *use-whatclib*
       (executable-target-p target-node)
       (null (getattr :lib-addr-map (target target-node)))))
       



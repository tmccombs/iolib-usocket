;;; conditions.lisp

(in-package :iolib-usocket)

(defun map-socket-error (sock-err)
  (let ((err (map-errno-error (sockets:error-code sock-err))))
    (if err
        err
        (make-condition 'unknown-error :real-error sock-err
                        :errno (sockets:error-code sock-err)))))

(defun map-resolver-error (resolver-err)
  (make-condition 'ns-unknown-error :real-error resolver-err))

(defparameter +iolib-error-map+
  `((sockets:socket-address-family-not-supported-error . protocol-not-supported-error)
    (sockets:socket-address-in-use-error . address-in-use-error)
    (sockets:socket-address-not-available-error . address-not-available-error)
    (sockets:socket-already-connected-error . unknown-error) ;todo
    (sockets:socket-connection-aborted-error . connection-aborted-error)
    (sockets:socket-connection-refused-error . connection-refused-error)
    (sockets:socket-connection-reset-error . connection-reset-error)
    (sockets:socket-connection-timeout-error . timeout-error)
    (sockets:socket-endpoint-shutdown-error . shutdown-error)
    (sockets:socket-host-down-error . host-down-error)
    (sockets:socket-host-unreachable-error . host-unreachable-error)
    (sockets:socket-network-down-error . network-down-error)
    (sockets:socket-network-unreachable-error . network-unreachable-error)
    (sockets:socket-no-buffer-space-error . no-buffers-error)
    (sockets:socket-no-network-error . network-unreachable-error)
    (sockets:socket-not-connected-error . operation-not-permitted-error)
    (sockets:socket-operation-not-supported-error . operation-not-supported-error)
    (sockets:socket-option-not-supported-error. operation-not-supported-error)
    (sockets:socket-error . ,#'map-socket-error)

    (sockets:resolver-fail-error . ns-no-recovery-error)
    (sockets:resolver-again-error . ns-try-again-condition)
    (sockets:resolver-no-name-error . ns-host-not-found-error)
    (sockets:resolver-error . ,#'map-resolver-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (serious-condition (let* ((usock-error (cdr (assoc (type-of condition)
                                                       +iolib-error-map+)))
                              (usock-error (if (functionp usock-error)
                                               (funcall usock-error condition)
                                               usock-error)))
                         (when usock-error
                           (error usock-error :socket socket))))
    (condition (let* ((usock-cond (cdr (assoc (type-of condition)
                                              +iolib-condition-map+)))
                      (usock-cond (if (functionp usock-cond)
                                      (funcall usock-cond condition)
                                      usock-cond)))
                 (when usock-cond
                   (signal usock-cond :socket socket))))))

(defmacro with-mapped-iolib-conditions ((&optional socket) &body body)
  `(handler-bind ((condition #'(lambda (c)
                                 (handle-condition c ,socket))))
     ,@body))

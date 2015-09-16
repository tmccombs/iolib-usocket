;;; iolib-usocket.lisp
;;;
;;; Copyright (c) 2015 Thayne McCombs

(in-package :iolib-usocket)

(defmethod socket ((socket sockets:socket))
  socket)

(defmethod socket-stream ((socket sockets:active-socket))
  socket)

(defmethod connected-p ((socket sockets:datagram-socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:socket-connected-p socket)))

;;TODO: figure out how to deal with wait lists and state

(defmethod socket-accept ((socket sockets:passive-socket) &key element-type)
  (declare (ignore element-type))       ; iolib sockets are bivalent
  (with-mapped-iolib-conditions (socket)
    (sockets:accept-connection socket)))

(defmethod socket-send ((socket sockets:datagram-socket) buffer length &key host port filename)
  (with-mapped-iolib-conditions (socket)
    (sockets:send-to socket buffer
                     :end length
                     :remote-host host
                     :remote-port port
                     :remote-filename filename)))

(defmethod socket-receive ((socket socket:datagram-socket) buffer length)
  (with-mapped-iolib-conditions (socket)
    (sockets:receive-from socket
                          :buffer buffer
                          :size length)))

(defmethod socket-close ((socket sockets:socket))
  (with-mapped-iolib-conditions (socket)
    (close socket)))

(defmethod get-local-name ((socket sockets:socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:local-name socket)))

(defmethod get-local-address ((socket sockets:internet-socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:local-host socket)))

(defmethod get-local-port ((socket sockets:internet-socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:local-port socket)))

(defmethod get-peer-name ((socket sockets:socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:remote-name socket)))

(defmethod get-peer-host ((socket sockets:internet-socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:remote-host socket)))

(defmethod get-peer-port ((socket sockets:internet-socket))
  (with-mapped-iolib-conditions (socket)
    (sockets:remote-port socket)))

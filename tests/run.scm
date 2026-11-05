(import test
        mosquitto
        (mosquitto foreign)
        (chicken condition))

(cond-expand
  (chicken-6 (import (rename (only (chicken bytevector)
                                   string->utf8 utf8->string)
                             (string->utf8 string->blob)
                             (utf8->string blob->string))))
  (chicken-5 (import (only (chicken blob) string->blob blob->string))))


(test-assert "make"
  (mqtt-client? (make-mqtt-client)))

(test "set string will" 0
  (mqtt-will-set (make-mqtt-client) "status" "offline"))

(test "set blob will" 0
  (mqtt-will-set (make-mqtt-client)
                 "status"
                 (string->blob (string #\o #\nul #\f))))

(test-assert "connect-lookup"
  (handle-exceptions exn
      ((condition-predicate 'err-eai) exn)
    (mqtt-connect (make-mqtt-client) "unexisted.unexisted.unexisted")))

(test-assert "connect-wrong-port"
  (handle-exceptions exn
      ((condition-predicate 'err-inval) exn)
    (mqtt-connect (make-mqtt-client) "localhost" #:port 9999999)))

(test-assert "connect-refused"
  (handle-exceptions exn
      ((condition-predicate 'err-errno) exn)
    (mqtt-connect (make-mqtt-client) "localhost" #:port 65000)))

(test-assert "socket-not-connected"
  (handle-exceptions exn
      ((condition-predicate 'mqtt) exn)
    (mqtt-socket (make-mqtt-client))
    #f))


(test-assert "set-callbacks"
  (let ((client (make-mqtt-client #:on-connect (lambda (client err)
                                                 (if err
                                                     (abort err)
                                                     (display "Yay, we are connected!"))))))
    (set-mqtt-client-disconnect-callback! client (lambda (cl unexpected?)
                                                   (when unexpected?
                                                     (display "Unexpected disconnect :'("))))
    (set-mqtt-client-message-callback! client (lambda (cl msg)
                                                (display (string->append "Topic: " (mqtt-message-topic msg)
                                                                         "Payload:" (blob->string (mqtt-message-payload msg))))
                                                (mqtt-publish client "topic2" "message received, thanks!" )))))

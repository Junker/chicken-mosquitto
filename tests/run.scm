(import test
        mosquitto
        (mosquitto foreign)
        (chicken condition))


(test-assert "make"
  (mqtt-client? (make-mqtt-client)))

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

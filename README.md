# chicken-mosquitto

Chicken Scheme Bindings to mosquitto MQTT client library

## Requirements

- [libmosquitto](https://mosquitto.org) installed


## Instalation

```bash
chicken-install mosquitto
```


## Usage

```scheme
(let ((client (make-mqtt-client #:on-connect
                                (lambda (client err)
                                  (if err
                                      (abort err)
                                      (display "Yay, we are connected!"))))))
  (set-mqtt-client-disconnect-callback! client
                                        (lambda (cl unexpected?)
                                          (when unexpected?
                                            (display "Unexpected disconnect..."))))

  (set-mqtt-client-message-callback! client
                                     (lambda (cl msg)
                                       (display (string->append
                                                 "Topic: " (mqtt-message-topic msg)
                                                 "Payload:" (blob->string (mqtt-message-payload msg))))
                                       (mqtt-publish client "topic2" "message received, thanks!" )))
  (mqtt-connect client "localhost" #:username "mqtt-admin" #:password "mypass")
  (mqtt-subscribe client "topic1")
  (mqtt-loop-forever client))
```
  
## Documentation

[Full Documentation](http://wiki.call-cc.org/eggref/5/mosquitto)

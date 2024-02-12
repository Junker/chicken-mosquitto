# chicken-mosquitto

Chicken Scheme Bindings to mosquitto MQTT client library

## Requirements

- [libmosquitto](https://mosquitto.org) installed


## Instalation

```bash
chicken-install mosquitto
```


## Usage

Example client which sends message into `topic2` each time it gets message from `topic1`:

```scheme
(import (chicken blob)
	(chicken string)
	(mosquitto))

(define (message message)
  (display message)
  (newline)
  (flush-output))

(define client
  (make-mqtt-client #:on-connect (lambda (client err)
				   (when err
				     (abort err))
				   (message "Yay, we are connected!")
				   (mqtt-subscribe client "topic1"))))

(set-mqtt-client-disconnect-callback! client
				      (lambda (cl unexpected?)
					(when unexpected?
					  (message "Unexpected disconnect..."))))

(set-mqtt-client-message-callback! client
				   (lambda (cl msg)
				     (message (string-append
					       "Topic: " (mqtt-message-topic msg) " "
					       "Payload:" (blob->string (mqtt-message-payload msg))))
				     (mqtt-publish client "topic2" "message received, thanks!" )))

(mqtt-connect client "localhost"
	      #:username "mqtt-admin"
	      #:password "mypass")

(mqtt-loop-forever client)
```

## Documentation

[Full Documentation](http://wiki.call-cc.org/eggref/5/mosquitto)

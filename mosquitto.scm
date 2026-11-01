(include "mosquitto-foreign.scm")
(module mosquitto (make-mqtt-client
                   mqtt-client
                   mqtt-client?
                   mqtt-client-mosquitto
                   mqtt-client-user-data
                   mqtt-client-connect-callback
                   mqtt-client-disconnect-callback
                   mqtt-client-publish-callback
                   mqtt-client-message-callback
                   mqtt-client-subscribe-callback
                   mqtt-client-unsubscribe-callback
                   mqtt-client-log-callback
                   set-mqtt-client-connect-callback!
                   set-mqtt-client-disconnect-callback!
                   set-mqtt-client-publish-callback!
                   set-mqtt-client-message-callback!
                   set-mqtt-client-subscribe-callback!
                   set-mqtt-client-unsubscribe-callback!
                   set-mqtt-client-log-callback!
                   mqtt-message
                   mqtt-message?
                   mqtt-message-id
                   mqtt-message-topic
                   mqtt-message-payload
                   mqtt-message-qos
                   mqtt-message-retain
                   mqtt-connect
                   mqtt-reinitialise
                   mqtt-disconnect
                   mqtt-loop-forever
                   mqtt-loop
                   mqtt-loop-read
                   mqtt-loop-write
                   mqtt-loop-misc
                   mqtt-socket
                   mqtt-want-write?
                   mqtt-will-set
                   mqtt-publish
                   mqtt-subscribe
                   mqtt-unsubscribe
                   mqtt-mosquitto-lib-version
                   mqtt-topic-matches-sub)
  (import scheme
          (chicken base)
          (chicken foreign)
          (chicken condition)
          (chicken locative)
          (chicken blob)
          (chicken type)
          (only srfi-1 alist-delete!)
          (only (chicken gc) set-finalizer!)
          (only (chicken memory) pointer->address move-memory!)
          (mosquitto foreign))

  (define-type mqtt-client (struct mqtt-client))
  (define-type mqtt-message (struct mqtt-message))
  (define-type connect-callback (or false (mqtt-client (or false condition) -> *)))
  (define-type disconnect-callback (or false (mqtt-client boolean -> *)))
  (define-type mid-callback (or false (mqtt-client fixnum -> *)))
  (define-type message-callback (or false (mqtt-client mqtt-message -> *)))
  (define-type log-callback (or false (mqtt-client (or false symbol) string -> *)))

  (: %make-mqtt-client (pointer * -> mqtt-client))
  (: mqtt-client? (* -> boolean : mqtt-client))
  (: mqtt-client-mosquitto (mqtt-client --> pointer))
  (: mqtt-client-user-data (mqtt-client --> *))
  (: mqtt-client-connect-callback (mqtt-client -> connect-callback))
  (: mqtt-client-disconnect-callback (mqtt-client -> disconnect-callback))
  (: mqtt-client-publish-callback (mqtt-client -> mid-callback))
  (: mqtt-client-message-callback (mqtt-client -> message-callback))
  (: mqtt-client-subscribe-callback (mqtt-client -> mid-callback))
  (: mqtt-client-unsubscribe-callback (mqtt-client -> mid-callback))
  (: mqtt-client-log-callback (mqtt-client -> log-callback))
  (: set-mqtt-client-connect-callback! (mqtt-client connect-callback -> undefined))
  (: set-mqtt-client-disconnect-callback! (mqtt-client disconnect-callback -> undefined))
  (: set-mqtt-client-publish-callback! (mqtt-client mid-callback -> undefined))
  (: set-mqtt-client-message-callback! (mqtt-client message-callback -> undefined))
  (: set-mqtt-client-subscribe-callback! (mqtt-client mid-callback -> undefined))
  (: set-mqtt-client-unsubscribe-callback! (mqtt-client mid-callback -> undefined))
  (: set-mqtt-client-log-callback! (mqtt-client log-callback -> undefined))

  (define-record-type mqtt-client
    (%make-mqtt-client mosquitto user-data)
    mqtt-client?
    (mosquitto mqtt-client-mosquitto)
    (user-data mqtt-client-user-data)
    (connect-callback mqtt-client-connect-callback set-mqtt-client-connect-callback!)
    (disconnect-callback mqtt-client-disconnect-callback set-mqtt-client-disconnect-callback!)
    (publish-callback mqtt-client-publish-callback set-mqtt-client-publish-callback!)
    (message-callback mqtt-client-message-callback set-mqtt-client-message-callback!)
    (subscribe-callback mqtt-client-subscribe-callback set-mqtt-client-subscribe-callback!)
    (unsubscribe-callback mqtt-client-unsubscribe-callback set-mqtt-client-unsubscribe-callback!)
    (log-callback mqtt-client-log-callback set-mqtt-client-log-callback!))

  (define *clients* '())

  (: make-mqtt-message (fixnum string * fixnum boolean --> mqtt-message))
  (: mqtt-message? (* -> boolean : mqtt-message))
  (: mqtt-message-id (mqtt-message --> fixnum))
  (: mqtt-message-topic (mqtt-message --> string))
  (: mqtt-message-payload (mqtt-message --> *))
  (: mqtt-message-qos (mqtt-message --> fixnum))
  (: mqtt-message-retain (mqtt-message --> boolean))

  (define-record-type mqtt-message
    (make-mqtt-message id topic payload qos retain)
    mqtt-message?
    (id mqtt-message-id)
    (topic mqtt-message-topic)
    (payload mqtt-message-payload)
    (qos mqtt-message-qos)
    (retain mqtt-message-retain))

  (define *log-levels*
    (list (cons +mosq-log-info+ 'log-info)
          (cons +mosq-log-notice+ 'log-notice)
          (cons +mosq-log-warning+ 'log-warning)
          (cons +mosq-log-err+ 'log-err)
          (cons +mosq-log-debug+ 'log-debug)))

  ;; EXTERNAL CALLBACKS

  (define-external (connect_cb (mosquitto-ptr mosq)
                               (c-pointer user-data)
                               (int rc))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-connect-callback client)))
      (when (procedure? callback)
        (callback client (connack-rc->condition rc)))))


  (define-external (disconnect_cb (mosquitto-ptr mosq)
                                  (c-pointer user-data)
                                  (int rc))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-disconnect-callback client)))
      (when (procedure? callback)
        (callback client (positive? rc)))))


  (define-external (publish_cb (mosquitto-ptr mosq)
                               (c-pointer user-data)
                               (int mid))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-publish-callback client)))
      (when (procedure? callback)
        (callback client mid))))


  (define-external (message_cb (mosquitto-ptr mosq)
                               (c-pointer user-data)
                               ((const message-ptr) msg))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-message-callback client)))
      (when (procedure? callback)
        (callback client (message-ptr->mqtt-message msg)))))


  (define-external (subscribe_cb (mosquitto-ptr mosq)
                                 (c-pointer user-data)
                                 (int mid)
                                 (int qos-count)
                                 ((c-pointer (const int)) granted-qos))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-subscribe-callback client)))
      (when (procedure? callback)
        (callback client mid))))


  (define-external (unsubscribe_cb (mosquitto-ptr mosq)
                                   (c-pointer user-data)
                                   (int mid))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-unsubscribe-callback client)))
      (when (procedure? callback)
        (callback client mid))))


  (define-external (log_cb (mosquitto-ptr mosq)
                           (c-pointer user-data)
                           (int level)
                           (c-string str))
    void
    (let* ((client (mosquitto-ptr-mqtt-client mosq))
           (callback (mqtt-client-log-callback client)))
      (when (procedure? callback)
        (callback client
                  (alist-ref level *log-levels*)
                  str))))


  ;;  =====
  ;;  ===== PRIV
  ;;  =====

  (: rc->symbol (fixnum --> symbol))
  (define (rc->symbol rc)
    (cond [(rassoc rc *mosq-errors*) => car]
          [else 'mosq-err-unknown]))

  (: connack-rc->symbol (fixnum --> symbol))
  (define (connack-rc->symbol rc)
    (cond [(rassoc rc *connack-errors*) => car]
          [else 'connack-unknown]))

  (: connack-rc->condition (fixnum --> (or false condition)))
  (define (connack-rc->condition rc)
    (if (eq? rc +connack-accepted+)
        #f
        (make-composite-condition
         (make-property-condition 'exn 'message (%mosquitto-connack-string rc))
         (make-property-condition 'mqtt)
         (make-property-condition (connack-rc->symbol rc)))))

  (: rc->condition (fixnum #!optional string --> (or false condition)))
  (define (rc->condition rc #!optional (err-prefix ""))
    (if (eq? rc +mosq-err-success+)
        #f
        (make-composite-condition
         (make-property-condition 'exn 'message (string-append err-prefix (%mosquitto-strerror rc)))
         (make-property-condition 'mqtt)
         (make-property-condition (rc->symbol rc)))))

  (: rc-assert (fixnum #!optional string -> fixnum))
  (define (rc-assert rc #!optional (err-prefix ""))
    (if (not (eq? rc +mosq-err-success+))
        (abort (rc->condition rc err-prefix))
        rc))

  (: fd-assert (fixnum -> fixnum))
  (define (fd-assert rc)
    (if (eq? rc -1)
        (abort
         (make-composite-condition
          (make-property-condition 'exn 'message "mosquitto_socket() failed")
          (make-property-condition 'mqtt)))
        rc))

  (: errno-assert ((or false pointer) -> pointer))
  (define (errno-assert rc)
    (if (not rc)
        (error (%errno-strerror (foreign-value "errno" int)))
        rc))

  (: mqtt-client-finalize (mqtt-client -> undefined))
  (define (mqtt-client-finalize client)
    (set! *clients*
      (alist-delete! (pointer->address (mqtt-client-mosquitto client))
                     *clients*))
    (%mosquitto-destroy (mqtt-client-mosquitto client)))

  (: mosquitto-ptr-mqtt-client (pointer -> mqtt-client))
  (define (mosquitto-ptr-mqtt-client ptr)
    (locative->object (cdr (assoc (pointer->address ptr)
                                  *clients*))))

  (: register-mqtt-client (mqtt-client -> undefined))
  (define (register-mqtt-client client)
    (set! *clients*
      (append *clients*
              (list (cons (pointer->address (mqtt-client-mosquitto client))
                          (make-weak-locative client))))))


  (: message-ptr->mqtt-message (pointer -> mqtt-message))
  (define (message-ptr->mqtt-message msg-ptr)
    (let* ((payload-len (%mosquitto-message-payloadlen msg-ptr))
           (payload-blob (void)))
      (unless (zero? payload-len)
        (set! payload-blob (make-blob payload-len))
        (move-memory! (%mosquitto-message-payload msg-ptr) payload-blob payload-len))
      (make-mqtt-message (%mosquitto-message-mid msg-ptr)
                         (%mosquitto-message-topic msg-ptr)
                         payload-blob
                         (%mosquitto-message-qos msg-ptr)
                         (%mosquitto-message-retain msg-ptr))))


  ;; =======
  ;; ======= PUBLIC
  ;; =======

  (: make-mqtt-client (#!rest -> mqtt-client))
  (define (make-mqtt-client #!key id (clean-session #t) user-data
                            on-connect on-disconnect on-publish on-message
                            on-subscribe on-unsubscribe on-log)
    (let* ((mosq-ptr (errno-assert (%mosquitto-new id clean-session #f)))
           (client (%make-mqtt-client mosq-ptr user-data)))
      (register-mqtt-client client)
      (set-finalizer! client mqtt-client-finalize)
      ;; bind external callbacks
      (%mosquitto-connect-callback-set mosq-ptr (location connect_cb))
      (%mosquitto-disconnect-callback-set mosq-ptr (location disconnect_cb))
      (%mosquitto-publish-callback-set mosq-ptr (location publish_cb))
      (%mosquitto-message-callback-set mosq-ptr (location message_cb))
      (%mosquitto-subscribe-callback-set mosq-ptr (location subscribe_cb))
      (%mosquitto-unsubscribe-callback-set mosq-ptr (location unsubscribe_cb))
      (%mosquitto-log-callback-set mosq-ptr (location log_cb))
      ;; attach user callbacks
      (set-mqtt-client-connect-callback! client on-connect)
      (set-mqtt-client-disconnect-callback! client on-disconnect)
      (set-mqtt-client-publish-callback! client on-publish)
      (set-mqtt-client-message-callback! client on-message)
      (set-mqtt-client-subscribe-callback! client on-subscribe)
      (set-mqtt-client-unsubscribe-callback! client on-unsubscribe)
      (set-mqtt-client-log-callback! client on-log)
      client))

  (: mqtt-reinitialise (mqtt-client #!rest -> fixnum))
  (define (mqtt-reinitialise client #!key id (clean-session #t))
    (%mosquitto-reinitialise (mqtt-client-mosquitto client) id clean-session #f))

  (: mqtt-connect (mqtt-client string #!rest -> fixnum))
  (define (mqtt-connect client host
                        #!key (port 1883) (keepalive 5)
                        bind-address username password
                        tls-cafile tls-capath tls-certfile tls-keyfile tls-insecure
                        tls-ocsp-required tls-use-os-certs tls-alpn
                        socks5-host (socks5-port 1080) socks5-username socks5-password
                        (reconnect-delay 1) (reconnect-delay-max 10) reconnect-exp-backoff
                        tcp-nodelay)
    (let ((mosq (mqtt-client-mosquitto client)))
      (when username
        (rc-assert (%mosquitto-username-pw-set mosq username password)
                   "Username/Password: "))
      (when (or tls-cafile tls-capath tls-certfile tls-keyfile)
        (assert (or tls-cafile tls-capath))
        (assert (or (and tls-certfile tls-keyfile)
                    (not (or tls-certfile tls-keyfile))))
        (rc-assert (%mosquitto-tls-set mosq tls-cafile tls-capath tls-certfile tls-keyfile #f)
                   "TLS: "))
      (when tls-insecure
        (rc-assert (%mosquitto-tls-insecure-set mosq #t)
                   "tls-insecure: "))
      (when tls-ocsp-required
        (rc-assert (%mosquitto-int-option mosq (foreign-value "MOSQ_OPT_TLS_OCSP_REQUIRED" int) 1)
                   "tls-ocsp-required: "))
      (when tls-use-os-certs
        (rc-assert (%mosquitto-int-option mosq (foreign-value "MOSQ_OPT_TLS_USE_OS_CERTS" int) 1)
                   "tls-use-os-certs: "))
      (when tls-alpn
        (rc-assert (%mosquitto-string-option mosq (foreign-value "MOSQ_OPT_TLS_ALPN" int) tls-alpn)
                   "tls-alpn: "))
      (when bind-address
        (rc-assert (%mosquitto-string-option mosq (foreign-value "MOSQ_OPT_BIND_ADDRESS" int) bind-address)
                   "bind-address: "))
      (when socks5-host
        (rc-assert (%mosquitto-socks5-set mosq socks5-host socks5-port socks5-username socks5-password)
                   "SOCKS5: "))
      (when tcp-nodelay
        (rc-assert (%mosquitto-int-option mosq (foreign-value "MOSQ_OPT_TCP_NODELAY" int) 1)
                   "tcp-nodelay: "))
      (rc-assert (%mosquitto-reconnect-delay-set mosq reconnect-delay reconnect-delay-max reconnect-exp-backoff)
                 "Reconnect options: ")
      (rc-assert (%mosquitto-connect mosq host port keepalive))))

  (: mqtt-disconnect (mqtt-client -> fixnum))
  (define (mqtt-disconnect client)
    (%mosquitto-disconnect (mqtt-client-mosquitto client)))

  (: mqtt-mosquitto-lib-version (--> fixnum fixnum fixnum))
  (define (mqtt-mosquitto-lib-version)
    (let-location ((major int)
                   (minor int)
                   (rev int))
      (%mosquitto-lib-version (location major) (location minor) (location rev))
      (values major minor rev)))

  (: mqtt-loop-forever (mqtt-client #!optional fixnum -> fixnum))
  (define (mqtt-loop-forever client #!optional (timeout 1000))
    (rc-assert (%mosquitto-loop-forever (mqtt-client-mosquitto client) timeout 1)))


  (: mqtt-loop (mqtt-client #!optional fixnum -> fixnum))
  (define (mqtt-loop client #!optional (timeout 1000))
    (rc-assert (%mosquitto-loop (mqtt-client-mosquitto client) timeout 1)))

  (: mqtt-loop-read (mqtt-client -> fixnum))
  (define (mqtt-loop-read client)
    (rc-assert (%mosquitto-loop-read (mqtt-client-mosquitto client) 1)))

  (: mqtt-loop-write (mqtt-client -> fixnum))
  (define (mqtt-loop-write client)
    (rc-assert (%mosquitto-loop-write (mqtt-client-mosquitto client) 1)))

  (: mqtt-loop-misc (mqtt-client -> fixnum))
  (define (mqtt-loop-misc client)
    (rc-assert (%mosquitto-loop-misc (mqtt-client-mosquitto client))))

  (: mqtt-socket (mqtt-client -> fixnum))
  (define (mqtt-socket client)
    (fd-assert (%mosquitto-socket (mqtt-client-mosquitto client))))

  (: mqtt-want-write? (mqtt-client -> boolean))
  (define  (mqtt-want-write? client)
    (%mosquitto-want-write (mqtt-client-mosquitto client)))

  (: mqtt-will-set (mqtt-client string (or string blob) #!rest -> fixnum))
  (define (mqtt-will-set client topic payload
                         #!key (qos 0) retain)
    (let ((payload-blob (cond
                         ((blob? payload) payload)
                         ((string? payload) (string->blob payload))
                         (else (error "payload type must be blob or string")))))
      (rc-assert
       (%mosquitto-will-set (mqtt-client-mosquitto client)
                            topic (blob-size payload-blob) payload-blob qos retain))))

  (: mqtt-publish (mqtt-client string (or string blob) #!rest -> fixnum))
  (define (mqtt-publish client topic payload
                        #!key (qos 0) retain)
    (let ((payload-blob (cond
                         ((blob? payload) payload)
                         ((string? payload) (string->blob payload))
                         (else (error "payload type must be blob or string")))))
      (let-location ((id int))
        (rc-assert
         (%mosquitto-publish (mqtt-client-mosquitto client)
                             (location id) topic (blob-size payload-blob) payload-blob qos retain))
        id)))

  (: mqtt-subscribe (mqtt-client string #!rest -> fixnum))
  (define (mqtt-subscribe client sub #!key (qos 0))
    (let-location ((id int))
      (rc-assert
       (%mosquitto-subscribe (mqtt-client-mosquitto client)
                             (location id) sub qos))
      id))

  (: mqtt-unsubscribe (mqtt-client string -> fixnum))
  (define (mqtt-unsubscribe client sub)
    (let-location ((id int))
      (rc-assert
       (%mosquitto-unsubscribe (mqtt-client-mosquitto client)
                               (location id) sub))
      id))

  (: mqtt-topic-matches-sub (string string -> boolean))
  (define (mqtt-topic-matches-sub sub topic)
    (let-location ((result bool))
      (rc-assert
       (%mosquitto-topic-matches-sub sub topic (location result)))
      result)))

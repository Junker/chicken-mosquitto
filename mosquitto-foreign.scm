(module (mosquitto foreign) *
  (import scheme
          (chicken base)
          (chicken foreign)
          (chicken condition)
          (chicken locative)
          (only (chicken string) string-translate))
  (import-for-syntax (only (chicken string) string-translate))

  ;; MOSQUITTO API: https://mosquitto.org/api/files/mosquitto-h.html

  (foreign-declare "#include <mosquitto.h>")
  (foreign-declare "#include <mqtt_protocol.h>")

  (foreign-code "mosquitto_lib_init();")

  (define-foreign-type mosquitto-ptr (c-pointer "struct mosquitto"))
  (define-foreign-type message-ptr (c-pointer "struct mosquitto_message"))
  (define-foreign-type mosq-opt (enum "mosq_opt_t"))

  (define +mosq-log-info+ (foreign-value "MOSQ_LOG_INFO" int))
  (define +mosq-log-notice+ (foreign-value "MOSQ_LOG_NOTICE" int))
  (define +mosq-log-warning+ (foreign-value "MOSQ_LOG_WARNING" int))
  (define +mosq-log-err+ (foreign-value "MOSQ_LOG_ERR" int))
  (define +mosq-log-debug+ (foreign-value "MOSQ_LOG_DEBUG" int))

  (define-syntax define-mfun
    (er-macro-transformer
     (lambda (exp r c)
       (let ((ret (cadr exp))
             (name (caddr exp))
             (args (cdddr exp)))
         `(,(r 'define) ,(string->symbol (string-append "%" (string-translate name "_" "-")))
           (,(r 'foreign-safe-lambda) ,ret ,name ,@args))))))

  (define-syntax define-mcb
    (er-macro-transformer
     (lambda (exp r c)
       (let (
             (name (cadr exp))
             (args (cddr exp)))
         `(,(r 'define) ,(string->symbol (string-append "%" (string-translate name "_" "-")))
           (,(r 'foreign-lambda*) void ((mosquitto-ptr mosq_ptr)
                                        ((function void ,@args) cb))
            ,(string-append name "(mosq_ptr, cb);")))))))

  (define +mosq-err-success+ (foreign-value "MOSQ_ERR_SUCCESS" int))
  (define *mosq-errors*
    `((err-auth-continue . ,(foreign-value "MOSQ_ERR_AUTH_CONTINUE" int))
      (err-no-subscribers . ,(foreign-value "MOSQ_ERR_NO_SUBSCRIBERS" int))
      (err-sub-exists . ,(foreign-value "MOSQ_ERR_SUB_EXISTS" int))
      (err-conn-pending . ,(foreign-value "MOSQ_ERR_CONN_PENDING" int))
      (err-nomem . ,(foreign-value "MOSQ_ERR_NOMEM" int))
      (err-protocol . ,(foreign-value "MOSQ_ERR_PROTOCOL" int))
      (err-inval . ,(foreign-value "MOSQ_ERR_INVAL" int))
      (err-no-conn . ,(foreign-value "MOSQ_ERR_NO_CONN" int))
      (err-conn-refused . ,(foreign-value "MOSQ_ERR_CONN_REFUSED" int))
      (err-not-found . ,(foreign-value "MOSQ_ERR_NOT_FOUND" int))
      (err-conn-lost . ,(foreign-value "MOSQ_ERR_CONN_LOST" int))
      (err-tls . ,(foreign-value "MOSQ_ERR_TLS" int))
      (err-payload-size . ,(foreign-value "MOSQ_ERR_PAYLOAD_SIZE" int))
      (err-not-supported . ,(foreign-value "MOSQ_ERR_NOT_SUPPORTED" int))
      (err-auth . ,(foreign-value "MOSQ_ERR_AUTH" int))
      (err-acl-denied . ,(foreign-value "MOSQ_ERR_ACL_DENIED" int))
      (err-unknown . ,(foreign-value "MOSQ_ERR_UNKNOWN" int))
      (err-errno . ,(foreign-value "MOSQ_ERR_ERRNO" int))
      (err-eai . ,(foreign-value "MOSQ_ERR_EAI" int))
      (err-proxy . ,(foreign-value "MOSQ_ERR_PROXY" int))
      (err-plugin-defer . ,(foreign-value "MOSQ_ERR_PLUGIN_DEFER" int))
      (err-malformed-utf8 . ,(foreign-value "MOSQ_ERR_MALFORMED_UTF8" int))
      (err-keepalive . ,(foreign-value "MOSQ_ERR_KEEPALIVE" int))
      (err-lookup . ,(foreign-value "MOSQ_ERR_LOOKUP" int))
      (err-malformed-packet . ,(foreign-value "MOSQ_ERR_MALFORMED_PACKET" int))
      (err-duplicate-property . ,(foreign-value "MOSQ_ERR_DUPLICATE_PROPERTY" int))
      (err-tls-handshake . ,(foreign-value "MOSQ_ERR_TLS_HANDSHAKE" int))
      (err-qos-not-supported . ,(foreign-value "MOSQ_ERR_QOS_NOT_SUPPORTED" int))
      (err-oversize-packet . ,(foreign-value "MOSQ_ERR_OVERSIZE_PACKET" int))
      (err-ocsp . ,(foreign-value "MOSQ_ERR_OCSP" int))
      (err-timeout . ,(foreign-value "MOSQ_ERR_TIMEOUT" int))
      (err-retain-not-supported . ,(foreign-value "MOSQ_ERR_RETAIN_NOT_SUPPORTED" int))
      (err-topic-alias-invalid . ,(foreign-value "MOSQ_ERR_TOPIC_ALIAS_INVALID" int))
      (err-administrative-action . ,(foreign-value "MOSQ_ERR_ADMINISTRATIVE_ACTION" int))
      (err-already-exists . ,(foreign-value "MOSQ_ERR_ALREADY_EXISTS" int))))

  (define +connack-accepted+ (foreign-value "CONNACK_ACCEPTED" int))
  (define *connack-errors*
    `((connack-refused-protocol-version . ,(foreign-value "CONNACK_REFUSED_PROTOCOL_VERSION" int))
      (connack-refused-identifier-rejected . ,(foreign-value "CONNACK_REFUSED_IDENTIFIER_REJECTED" int))
      (connack-refused-server-unavailable . ,(foreign-value "CONNACK_REFUSED_SERVER_UNAVAILABLE" int))
      (connack-refused-bad-username-password . ,(foreign-value "CONNACK_REFUSED_BAD_USERNAME_PASSWORD" int))
      (connack-refused-not-authorized . ,(foreign-value "CONNACK_REFUSED_NOT_AUTHORIZED" int))))


  ;; =====
  ;; ===== DEFINE MOSQUITTO C FUNCTIONS
  ;; =====

  (define-mfun mosquitto-ptr "mosquitto_new" c-string bool c-pointer)
  (define-mfun void "mosquitto_destroy" mosquitto-ptr)
  (define-mfun int "mosquitto_lib_version" (c-pointer int) (c-pointer int) (c-pointer int))
  (define-mfun int "mosquitto_reinitialise" mosquitto-ptr c-string bool c-pointer)

  ;; Will
  (define-mfun int "mosquitto_will_set" mosquitto-ptr c-string int c-string int bool)
  (define-mfun int "mosquitto_will_clear" mosquitto-ptr)

  ;; Username and password
  (define-mfun int "mosquitto_username_pw_set" mosquitto-ptr c-string c-string)

  ;; Connecting, reconnecting, disconnecting
  (define-mfun int "mosquitto_connect" mosquitto-ptr c-string int int)
  (define-mfun int "mosquitto_connect_bind" mosquitto-ptr c-string int int c-string)
  (define-mfun int "mosquitto_connect_async" mosquitto-ptr c-string int int)
  (define-mfun int "mosquitto_connect_bind_async" mosquitto-ptr c-string int int c-string)
  (define-mfun int "mosquitto_disconnect" mosquitto-ptr)
  (define-mfun int "mosquitto_reconnect" mosquitto-ptr)
  (define-mfun int "mosquitto_reconnect_async" mosquitto-ptr)

  ;; Publishing, subscribing, unsubscribing
  (define-mfun int "mosquitto_publish" mosquitto-ptr (c-pointer int) (const c-string) int (const blob) int bool)
  (define-mfun int "mosquitto_subscribe" mosquitto-ptr c-pointer c-string int)
  (define-mfun int "mosquitto_unsubscribe" mosquitto-ptr c-pointer c-string)

  ;; Network loop
  (define-mfun int "mosquitto_loop_forever" mosquitto-ptr int int)
  (define-mfun int "mosquitto_loop" mosquitto-ptr int int)
  (define-mfun int "mosquitto_loop_start" mosquitto-ptr)
  (define-mfun int "mosquitto_loop_stop" mosquitto-ptr bool)
  (define-mfun int "mosquitto_loop_read" mosquitto-ptr int)
  (define-mfun int "mosquitto_loop_write" mosquitto-ptr int)
  (define-mfun int "mosquitto_loop_misc" mosquitto-ptr)
  (define-mfun bool "mosquitto_want_write" mosquitto-ptr)


  ;; SOCKS5 proxy functions
  (define-mfun int "mosquitto_socks5_set" mosquitto-ptr c-string int c-string c-string)

  ;; Client option
  (define-mfun int "mosquitto_int_option" mosquitto-ptr mosq-opt int)
  (define-mfun int "mosquitto_string_option" mosquitto-ptr mosq-opt c-string)
  (define-mfun int "mosquitto_reconnect_delay_set" mosquitto-ptr int int bool)

  ;; TLS support
  (define-mfun int "mosquitto_tls_set" mosquitto-ptr c-string c-string c-string c-string c-pointer)
  (define-mfun int "mosquitto_tls_insecure_set" mosquitto-ptr bool)
  (define-mfun int "mosquitto_tls_opts_set" mosquitto-ptr int c-string c-string)
  (define-mfun int "mosquitto_tls_psk_set" mosquitto-ptr c-string c-string c-string)

  ;; Callbacks
  (define-mcb "mosquitto_connect_callback_set" (mosquitto-ptr c-pointer int))
  (define-mcb "mosquitto_disconnect_callback_set" (mosquitto-ptr c-pointer int))
  (define-mcb "mosquitto_publish_callback_set" (mosquitto-ptr c-pointer int))
  (define-mcb "mosquitto_message_callback_set" (mosquitto-ptr c-pointer (const message-ptr)))
  (define-mcb "mosquitto_subscribe_callback_set" (mosquitto-ptr c-pointer int int (c-pointer (const int))))
  (define-mcb "mosquitto_unsubscribe_callback_set" (mosquitto-ptr c-pointer int))
  (define-mcb "mosquitto_log_callback_set" (mosquitto-ptr c-pointer int (const c-string)))

  ;; Utility functions
  (define-mfun c-string "mosquitto_strerror" int)
  (define-mfun c-string "mosquitto_connack_string" int)
  (define-mfun c-string "mosquitto_reason_string" int)
  ;; (define-mfun int "mosquitto_topic_matches_sub" c-string c-string (c-pointer bool))
  (define-mfun int "mosquitto_pub_topic_check" c-string)
  (define-mfun int "mosquitto_sub_topic_check" c-string)

  (define %errno-strerror
    (foreign-lambda c-string "strerror" int))

  ;; Message
  (define %mosquitto-message-mid
    (foreign-lambda* int ((message-ptr msg))
      "C_return(msg->mid);"))

  (define %mosquitto-message-topic
    (foreign-lambda* c-string ((message-ptr msg))
      "C_return(msg->topic);"))

  (define %mosquitto-message-payload
    (foreign-lambda* c-pointer ((message-ptr msg))
      "C_return(msg->payload);"))

  (define %mosquitto-message-payloadlen
    (foreign-lambda* int ((message-ptr msg))
      "C_return(msg->payloadlen);"))

  (define %mosquitto-message-qos
    (foreign-lambda* int ((message-ptr msg))
      "C_return(msg->qos);"))

  (define %mosquitto-message-retain
    (foreign-lambda* bool ((message-ptr msg))
      "C_return(msg->retain);")))

;;;; Web API for certs
(cl:in-package #:jfh-certs-web-app)

(web:define-api-endpoint daily-tip-data "/daily-tip-data" ()
  "REST endpoint for tip of the day"
  (case web:verb
    (:get
     (jfh-utility:serialize-to-json (list "api result one")))))

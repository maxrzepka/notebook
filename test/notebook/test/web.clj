(ns notebook.test.web
  (:require [notebook.web :as w]
            [net.cgrand.enlive-html :as h])
  (:use [kerodon core test]        
        [clojure.test]))

(deftest user-can-login-and-save-note
 ;imagine ring-app is a login required picture upload
  (-> (session w/routes)
      (visit "/login")
      (fill-in (h/attr= :name "user") "username")
      (fill-in (h/attr= :name "password") "any-password")
      ;;doesn't work because press (impl/form-with-submit ) catch only input submit, here it's a button tag
      (press "Sign in")
      (visit "/note")))
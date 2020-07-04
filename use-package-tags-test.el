;;; -*- lexical-binding: t -*-

(require 'buttercup)

(require 'use-package)
(require 'use-package-tags)

(setq use-package-always-ensure nil)

(add-to-list 'load-path (expand-file-name "tests/"
                                          (file-name-directory
                                           (or load-file-name
                                               (buffer-file-name)))))

(describe ":tags keyword"
  (it "enables packages without :tags"
    (expect (let ((use-package-tags-current-profile '(active)))
              (use-package test-a)
              (featurep 'test-a))
            :to-be-truthy))

  (it "enables packages with an active tag"
    (expect (let ((use-package-tags-current-profile '(active)))
              (use-package test-b
                :tags '(foo active bar))
              (featurep 'test-b))
            :to-be-truthy))

  (it "disables packages without an active tag"
    (expect (let ((use-package-tags-current-profile '(active)))
              (use-package test-c
                :tags '(foo inactive bar))
              (featurep 'test-c))
            :to-be nil)))

(describe "use-package-tags-select"
  (let ((result (use-package-tags-select '(active foo)
                  :from "./tests/init.el")))
    (it "selects packages without :tags keyword"
      (expect (member 'a result) :to-be-truthy))
    (it "selects packages with an active keyword"
      (expect (member 'b result) :to-be-truthy))
    (it "unselects packages withnout an active keyword"
      (expect (member 'c result) :to-be nil))
    (it "handles :disabled keyword"
      (expect (member 'disabled result) :to-be nil))
    (it "handles :if keyword"
      (expect (member 'test result) :to-be-truthy)
      (expect (member 'test-nil result) :to-be nil)))
  (it "selects all packages when t is given as query"
    (expect (use-package-tags-select t :from "./tests/init.el")
            :to-equal '(a b c test)))
  (it "supports :as keyword for specifying the output"
    (expect (use-package-tags-select t :from "./tests/init.el"
                                     :as 'symbols)
            :to-equal '(a b c test))
    (expect (use-package-tags-select t :from "./tests/init.el"
                                     :as 'strings)
            :to-equal '("a" "b" "c" "test"))
    (expect (use-package-tags-select t :from "./tests/init.el"
                                     :as 'lines)
            :to-equal "a\nb\nc\ntest")))

(provide 'use-package-tag-test)

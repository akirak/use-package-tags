(use-package a)

(use-package b :tags (foo active bar))

(use-package c :tags (inactive))

(use-package disabled
  :disabled t)

(use-package test
  :if t)

(use-package test-nil
  :if (identity nil))

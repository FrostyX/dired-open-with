;;; dired-open-with-tests.el --- Tests for dired-open-with -*- lexical-binding: t; -*-

(require 'ert)
(require 'dired-open-with)

(ert-deftest dired-open-with--mimetype ()
  (should (equal (dired-open-with--mimetype "/foo/bar/baz.png") "image/png")))

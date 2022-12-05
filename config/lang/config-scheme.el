;;; config-scheme.el --- Scheme                      -*- lexical-binding: t; -*-

(require 'use-package)

(use-package racket-mode
  :straight t)

(use-package quack
  :straight t)

(use-package geiser
  :straight t)

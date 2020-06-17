;;; config-browser.el --- Web browsing               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'use-package)
(require 'config-path)

;; * URL handling

(use-package browse-url
  :custom
  ;; Use Firefox Nightly on macOS
  (browse-url-browser-function #'browse-url-firefox)
  (browse-url-firefox-program "open")
  (browse-url-firefox-arguments '("-a" "Firefox Nightly")))

(provide 'config-browser)
;;; config-browser.el ends here

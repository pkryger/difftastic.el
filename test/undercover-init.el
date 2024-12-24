;;; undercover-init.el --- initialization for undercover -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'undercover nil t)

(when (featurep 'undercover)
  (undercover "*.el"
	          (:report-format 'lcov)
	          (:send-report nil)))

(provide 'undercover-init.el)

;;; undercover-init.el ends here

;;; undercover-init.el --- initialization for undercover
;;; Commentary:
;;; Code:
(when (require 'undercover nil t)
  (undercover "*.el"
	      (:report-format 'lcov)
	      (:send-report nil)))
;;; undercover-init.el ends here

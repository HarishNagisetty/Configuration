;; -*- lexical-binding: t -*-

(deftheme harish "Personal customizations over default theme.")

(custom-theme-set-faces
 'harish
 '(region ((t (:inherit nil :background "#d7d7ff"))))
 '(fringe ((t (:background "white")))))

(provide-theme 'harish)

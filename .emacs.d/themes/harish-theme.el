;; -*- lexical-binding: t -*-

(deftheme harish "Personal customizations over default theme.")

(let ((background-color "#fbf8ef")
      (foreground-color "#202020")
      (region-color "#d7d7ff")
      (act1 "#e7e5eb")
      (border "#b3b9be")
      (func "#6c3163"))
  (custom-theme-set-faces
   'harish
   `(region ((t (:inherit nil :background ,region-color))))
   `(fringe ((t (:background ,background-color))))
   `(default ((t (:background ,background-color :foreground ,foreground-color))))
   `(mode-line ((t (:foreground ,foreground-color :background ,act1 :box (:color ,border :line-width 1)))))
   `(mode-line-buffer-id ((t (:inherit bold :foreground ,func))))
   `(mode-line-inactive  ((t (:foreground ,foreground-color :background ,background-color :box (:color ,border :line-width 1)))))))

(provide-theme 'harish)

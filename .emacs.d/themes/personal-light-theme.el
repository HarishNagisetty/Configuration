;; -*- lexical-binding: t -*-

(deftheme personal-light "Personal theme inspired by Spacemacs and Bespoke.")

(let ((background-color "#fbf8ef")
      (foreground-color "#202020")
      (region-color     "#d7d7ff")
      (active-color     "#e7e5eb")
      (inactive-color   "#f5f5f5")
      (filename-color   "#282b35")
      (faded-color      "#ababab")
      (function-color   "#250361")
      (trail-ws-color   "#f0823e")
      (comment-fg-color "#49005c")
      (comment-bg-color "#fbf8ef")
      (salient-color    "#303db4")
      (purple-color     "#6c3163")
      (blue-color       "#3256a8")
      (red-color        "#a8326f")
      (green-color      "#32a852")
      (brown-color      "#a86432"))
  (custom-theme-set-faces
   'personal-light

   ;; Basic Faces
   `(default ((t :background ,background-color
                 :foreground ,foreground-color)))
   `(region ((t :background ,region-color)))
   `(fringe ((t :background ,background-color)))
   `(mode-line-buffer-id ((t :weight bold :foreground ,filename-color)))
   `(mode-line
     ((t :foreground ,foreground-color
         :background ,active-color
         :box (:line-width 3 :color ,active-color :style nil))))
   `(mode-line-inactive
     ((t :foreground ,foreground-color
         :background ,inactive-color
         :box (:line-width 3 :color ,inactive-color :style nil))))

   ;; Outline
   `(outline-1 ((t :inherit font-lock-function-name-face :height 1.3)))
   `(outline-2 ((t :inherit font-lock-variable-name-face :height 1.2)))
   `(outline-3 ((t :inherit font-lock-keyword-face :height 1.1)))

   ;; Whitespace
   `(whitespace-trailing ((t :background ,trail-ws-color)))
   ))

(provide-theme 'personal-light)

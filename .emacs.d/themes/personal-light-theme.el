;; -*- lexical-binding: t -*-

(deftheme personal-light "Personal theme inspired by Spacemacs and Bespoke.")

(let ((background-color "#fbf8ef")
      (foreground-color "#202020")
      (region-color     "#d7d7ff")
      (active-color     "#faa6a0")
      (inactive-color   "#ffcbc7")
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

   ;; Font-lock
   `(font-lock-comment-face ((t :foreground ,comment-fg-color
                                :background ,comment-bg-color
                                :weight bold)))
   `(font-lock-function-name-face ((t :foreground ,function-color
                                      :weight bold)))

   ;; Outline
   `(outline-1 ((t :foreground ,blue-color  :height 1.3)))
   `(outline-2 ((t :foreground ,green-color :height 1.2)))
   `(outline-3 ((t :foreground ,red-color   :height 1.1)))
   `(outline-4 ((t :foreground ,brown-color :height 1.0)))
   `(outline-5 ((t :foreground ,blue-color  :height 1.0)))
   `(outline-6 ((t :foreground ,green-color :height 1.0)))
   `(outline-7 ((t :foreground ,red-color   :height 1.0)))
   `(outline-8 ((t :foreground ,brown-color :height 1.0)))

   ;; Whitespace
   `(whitespace-trailing ((t :background ,trail-ws-color)))

   ;; Org
   `(org-document-title ((t :foreground ,salient-color :height 1.1)))
   `(org-done           ((t :foreground ,faded-color :strike-through t)))
   `(org-level-1        ((t :inherit outline-1)))
   `(org-level-2        ((t :inherit outline-2)))
   `(org-level-3        ((t :inherit outline-3)))
   `(org-level-4        ((t :inherit outline-4)))
   `(org-level-5        ((t :inherit outline-5)))
   `(org-level-6        ((t :inherit outline-6)))
   `(org-level-7        ((t :inherit outline-7)))
   `(org-level-8        ((t :inherit outline-8)))
   `(org-headline-done  ((t :foreground ,faded-color)))
   ))

(provide-theme 'personal-light)

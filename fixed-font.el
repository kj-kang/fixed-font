;;; fixed-font.el --- 고정폭 글꼴을 설정한다

;;; Commentary:
;;;

;;; Code:
;;;


(require 'seq)


(defcustom fixed-font-ascii-font "Envy Code R"
  "영문 글꼴의 이름을 지정한다."
  :type  'string
  :tag   'fixed-font
  :group 'fixed-font)


(defcustom fixed-font-non-ascii-font "D2Coding"
  "한글 글꼴의 이름을 지정한다."
  :type  'string
  :tag   'fixed-font
  :group 'fixed-font)


(defcustom fixed-font-default-height 160
  "글꼴의 기본 크기를 지정한다."
  :type  'int
  :tag   'fixed-font
  :group 'fixed-font)


(defvar fixed-font-rescale-alist
  '((100 . 1.05)
    (110 . 1.10)
    (120 . 1.05)
    (130 . 1.10)
    (140 . 1.15)
    (150 . 1.10)
    (160 . 1.15)
    (170 . 1.10)
    (180 . 1.15)
    (190 . 1.10)
    (200 . 1.10)))


(defun fixed-font--get-rescale (height)
  "영문 글꼴의 크기(HEIGHT)에 따른 한글 글꼴의 스케일을 반환한다."
  (cdr (car (last (seq-take-while (lambda (x) (<= (car x) height)) fixed-font-rescale-alist)))))


(defun fixed-font--set-height (height)
  "글꼴의 크기(HEIGHT)를 설정한다."
  (let ((rescale (fixed-font--get-rescale height)))
    (set-face-attribute 'default nil :family fixed-font-ascii-font)
    (set-fontset-font t 'hangul (font-spec :family fixed-font-non-ascii-font))
    (setq face-font-rescale-alist `((,fixed-font-non-ascii-font . ,rescale)))
    (set-face-attribute 'default nil :height height)))


;;;###autoload
(defun fixed-font-set-height-default ()
  "글꼴의 크기를 기본 크기로 지정한다."
  (interactive)
  (fixed-font--set-height fixed-font-default-height))


;;;###autoload
(defun fixed-font-increase-height ()
  "글꼴의 크기를 한단계(10) 높인다."
  (interactive)
  (let ((height (+ (face-attribute 'default :height) 10)))
    (fixed-font--set-height height)))

(defun fixed-font-decrease-height ()
  "글꼴의 크기를 한단계(10) 줄인다."
  (interactive)
  (let ((height (- (face-attribute 'default :height) 10)))
    (fixed-font--set-height height)))

(provide 'fixed-font)

;;; fixed-font.el ends here

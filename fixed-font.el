;;; fixed-font.el --- 고정폭 글꼴을 설정한다

;;; Commentary:
;;;

;;; Code:
;;;

(require 'seq)


(defcustom fixed-font-ascii-font "Monaco"
  "영문 글꼴의 이름을 지정한다."
  :type  'string
  :group 'fixed-font)


(defcustom fixed-font-non-ascii-font "D2Coding"
  "한글 글꼴의 이름을 지정한다."
  :type  'string
  :group 'fixed-font)


(defcustom fixed-font-default-height 160
  "글꼴의 기본 크기를 지정한다."
  :type  'int
  :group 'fixed-font)


(defvar fixed-font-rescale-alist
  '(("DejaVu Sans Mono" .
     ((100 . 1.25)
      (110 . 1.30)
      (120 . 1.20)
      (130 . 1.25)
      (140 . 1.15)
      (150 . 1.20)
      (160 . 1.25)
      (170 . 1.20)
      (180 . 1.25)
      (190 . 1.20)
      (200 . 1.20)))
    ("Envy Code R" .
     ((110 . 1.10)
      (120 . 1.05)
      (130 . 1.10)
      (140 . 1.15)
      (150 . 1.10)
      (160 . 1.15)
      (170 . 1.10)
      (180 . 1.15)
      (190 . 1.10)
      (200 . 1.10)))))


(defvar fixed-font-default-rescale-alist
  '((100 . 1.20)
    (110 . 1.30)
    (120 . 1.20)
    (130 . 1.25)
    (140 . 1.15)
    (150 . 1.20)
    (160 . 1.25)
    (170 . 1.20)
    (180 . 1.25)
    (190 . 1.20)
    (200 . 1.20)))


(defun fixed-font--get-rescale (ascii-font height)
  "영문 글꼴(ASCII-FONT)과 높이(HEIGHT)에 맞는 스케일을 반환한다.
만약 해당하는 스케일을 찾을 수 없다면 fixed-font-default-rescale-alist
  의 값을 참조하여 반환한다."
  (let* ((rescale-alist (assoc ascii-font fixed-font-rescale-alist))
	 (rescale       (assoc height rescale-alist)))
    (cdr (if (eq rescale-alist nil)
	(assoc height fixed-font-default-rescale-alist)
	rescale))))


(defun fixed-font--set-height (height)
  "글꼴의 높이(HEIGHT)를 설정한다."
  (let ((rescale (fixed-font--get-rescale fixed-font-ascii-font
					  height)))
    (message "font: %s, height: %d, scale: %.2f" fixed-font-ascii-font height rescale)
    (set-face-attribute 'default nil :family fixed-font-ascii-font)
    (set-fontset-font t 'hangul (font-spec :family fixed-font-non-ascii-font))
    (setq face-font-rescale-alist `((,fixed-font-non-ascii-font . ,rescale)))
    (set-face-attribute 'default nil :height height)))


;;;###autoload
(defun fixed-font-set-height-default ()
  "글꼴의 높이를 기본 크기로 지정한다."
  (interactive)
  (fixed-font--set-height fixed-font-default-height))


;;;###autoload
(defun fixed-font-increase-height ()
  "글꼴의 높이를 한단계(10) 높인다."
  (interactive)
  (let ((height (+ (face-attribute 'default :height) 10)))
    (fixed-font--set-height height)))


;;;###autoload
(defun fixed-font-decrease-height ()
  "글꼴의 높이를 한단계(10) 줄인다."
  (interactive)
  (let ((height (- (face-attribute 'default :height) 10)))
    (fixed-font--set-height height)))


(provide 'fixed-font)

;;; fixed-font.el ends here

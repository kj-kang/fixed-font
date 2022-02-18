;;; fixed-font.el --- ...
;;;
;;; Commentary:
;;;
;;; Code:
;;;
;;; 글꼴은 글자폭의 형태에 따라 가변폭과 고정폭으로 구분한다. 가변폭의 경우 "W"와 "I"의
;;; 가로폭(width)이 다르며, 고정폭은 같다는 차이점이 있다.
;;;
;;; 일반적으로 편한하게 글을 읽기에는 가변폭이 좋으나, 개발자이 코딩용으로는 가독성이
;;; 나쁜 경우가 종종 발생한다. 아래와 같은 테이블에 한글과 영문을 배치할 때 서로 폭이
;;; 다르기 때문에 읽기에 불편한 점이 있다.
;;;
;;; ----|------|
;;; 한글|테스트|
;;; abcd|abcdef|
;;; ----|------|
;;;
;;; 이 패키지는 한글 1글자의 폭과 영문 2글자의 폭을 맞출 수 있도록 도와준다.
;;;
;;; (use-pacakge fixed-font
;;;   :load-path "~/path/to/fixed-font"
;;;   :bind
;;;   ("C-0" . fixed-font-default))
;;;   ("C-=" . fixed-font-increase))
;;;   ("C--" . fixed-font-decrease)
;;;   :init
;;;   (setq fixed-font-ascii-font "Source Code Pro")
;;;   (setq fixed-font-hangul-font "NanumGothicCoding")
;;;   (setq fixed-font-default-height 100)
;;;   (fixed-font-default))
;;;


(defcustom fixed-font-ascii-font "Source Code Pro"
  "영문 글꼴을 지정한다."
  :type 'string
  :group 'fixed-font)

(defcustom fixed-font-hangul-font "NanumGothicCoding"
  "한글 글꼴을 지정한다."
  :type 'string
  :group 'fixed-font)

(defcustom fixed-font-default-hegiht 100
  "글꼴을 기본 크기를 지정한다."
  :type 'int
  :group 'fixed-font)

(defconst fixed-font-current-height fixed-font-default-hegiht
  "현재 글꼴의 크기를 저장한다.")

(defcustom fixed-font-rescale-alist
  '(( 80 . 1.30)
    ( 90 . 1.20)
    (100 . 1.20)
    (110 . 1.30)
    (120 . 1.20)
    (130 . 1.20)
    (140 . 1.20)
    (150 . 1.20)
    (160 . 1.20)
    (170 . 1.20)
    (180 . 1.20)
    (190 . 1.20)
    (200 . 1.20))
  "영문 글꼴 대비 한글 글꼴의 비율을 크기별로 지정한다."
  :group 'fixed-font)

(defun fixed-font--min-height ()
  "사용가능한 최소 크기를 반환한다."
  (seq-min
   (seq-map
    (lambda (height-rescale-pair) (cdr height-rescale-pair))
    fixed-font-rescale-alist)))

(defun fixed-font--max-height ()
  "사용가능한 최대 크기를 반환한다."
  (seq-max
   (seq-map
    (lambda (height-rescale-pair) (cdr height-rescale-pair))
    fixed-font-rescale-alist)))

(defun fixed-font--get-rescale-by-height (height)
  "주어진 글꼴의 크기(HEIGHT)에 해당하는 비율을 반환한다."
  (cdr (assoc height fixed-font-rescale-alist)))

(defun fixed-font--set-height (height)
  "주어진 글꼴의 크기(HEIGHT)를 반영한다."
  (let ((min-height (fixed-font--min-height))
        (max-height (fixed-font--max-height))
        (rescale    (fixed-font--get-rescale-by-height height)))
    (message "ascii-font: %s, hangul-font: %s, height: %d, rescale: %.3f" fixed-font-ascii-font fixed-font-hangul-font height rescale)
    (set-fontset-font t 'hangul (font-spec :family fixed-font-hangul-font))
    (set-face-attribute 'default nil :family fixed-font-ascii-font :height height)
    (setq face-font-rescale-alist `((,fixed-font-hangul-font . ,rescale)))
    (setq fixed-font-current-height height)
    ))

;;;###autoload
(defun fixed-font-default ()
  "글꼴의 크기를 기본값으로 변경한다."
  (interactive)
  (fixed-font--set-height fixed-font-default-hegiht))

;;;###autoload
(defun fixed-font-increase ()
  "글꼴의 크기를 한 단계(10) 크게 한다."
  (interactive)
  (let ((new-height (+ fixed-font-current-height 10)))
    (fixed-font--set-height new-height)))

;;;###autoload
(defun fixed-font-decrease ()
  "글꼴의 크기를 한 단계(10) 작게 한다."
  (interactive)
  (let ((new-height (- fixed-font-current-height 10)))
    (fixed-font--set-height new-height)))

(provide 'fixed-font)
;;; fixed-font.el ends here

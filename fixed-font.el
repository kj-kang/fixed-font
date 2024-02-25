;;; fixed-font -- 한글과 영문 글꼴의 고정폭 비율 설정
;;;
;;; Commentary:
;;;
;;;   글꼴은 글자폭의 형태에 따라 가변폭과 고정폭으로 구분한다.  가변폭의
;;; 경우 글자에 따라 폭(width)이 달라질 수 있다.  예를 들어 "W"와 "I"의
;;; 가로폭(width)이 다르며, 고정폭은 항상 같다.
;;;
;;;   가독성과 정확성을 요구하는 코딩의 글꼴은 고정폭을 사용하는 것이 좋다.
;;; 책이나 게시물을 읽기에는 고정폭 보다 가변폭 글꼴이 좋다고 알려져 있다.
;;; 그러나 가변폭 글꼴의 경우 'o', 'O', '0' 또는  '1', 'i', 'l', 'L' 등의
;;; 구분이 어려워 오타가 발생하기 쉽다.
;;;
;;;   고정폭 영문 글꼴은 'monaco', 'source code pro', casadia code',
;;;  'inconsolata', 'fira code' 등 다양한 글꼴을 선택하여 사용할 수 있다.
;;; 반면에 한글 글꼴은 '나눔고딕코딩, 'D2Coding' 정도로 선태지가 매우
;;; 좁다.
;;;;
;;;   한글 글꼴을 사용할 때 영문 글꼴을 다른 것으로 대체 할 수 있다면,
;;; 개인의 취향에 맞는 다양한 조합을 사용할 수 있다.  이 패키지는 한글과
;;; 영문 글꼴을 각각 선택하여 사용할 수 있도록 한다.
;;;
;;; (setq fixed-font-hangul-font "NanumGothicCode") ;; 한글 글꼴 설정
;;; (setq fixed-font-ascii-font "Ubuntu Mono")      ;; 영문 글꼴 설정
;;;
;;;   같은 고정폭 글꼴이라 하더다라고 글꼴마다 고정폭의 기준이 다르다.
;;; 한글과 영문 글꼴을 조합할 경우, 한글 한글자에 영문 두글자가 나오지 않는
;;; 경우가 빈번하다.  이 경우에는 글꼴의 비율(scale)을 조정해 줄 필요가 있다.
;;; 글꼴의 크기에 따라 비율이 다를 수 있기 때문에 각각 지정해야 한다.
;;;
;;; (add-to-list 'fixed-font-rescale-list
;;;   (("NanumGothicCoding" . "Ubuntu Mono")
;;;    ((70  . 1.20)         ;; 글꼴의 크기가 70일때 비율은 1.20
;;;     (80  . 1.30)
;;;     (90  . 1.25)
;;;     (100 . 1.20))))
;;;
;;;   아래와 같은 표를 하나 만들어 보면, 각 글꼴의 비율이 맞는지 틀린지
;;; 보다 쉽게 확인 할 수 있다.
;;;
;;; | 한글 | 테스트 |
;;; | ABCD | abcdef |
;;;
;;;   글꼴의 크기를 자유롭게 늘리거나 줄일 수 있도록 몇 가지 함수를 제공한다.
;;; 이 함수를 단추키에 할당하면 유용하게 사용할 수 있다.
;;;
;;; (gloalb-set-key (kdb "C-+") fixed-font-increase) ;; 글꼴의 크기를 한단계 늘린다
;;; (global-set-key (kbd "C--") fixed-font-decrease) ;; 글꼴의 크기를 한단계 줄인다
;;;
;;;   다음과 같이 use-package 를 사용하여 설정하는 것도 가능하다.
;;;
;;; (use-package fixed-font
;;;   :load-path "~/.emacs.d/site-lisp"
;;;   :bind
;;;   ("C-0" . fixed-font-default)
;;;   ("C-+" . fxied-font-increase)
;;;   ("C--" . fixedf-ont-decrease)
;;;   :custom
;;;   (fixed-font-hangul-font "NanumGothicCoding")
;;;   (fixed-font-ascii-font  "Ubuntu Moon")
;;;   (fixed-font-default-height 100)
;;;   :init
;;;   (fixed-font-default))
;;;
;;; Code:
;;;

(require 'seq)

(defcustom fixed-font-hangul-font "NanumGothicCoding"
  "한글 글꼴을 지정한다."
  :type 'string
  :group 'fixed-font)

(defcustom fixed-font-ascii-font "Monospace"
  "영문 글꼴을 지정한다."
  :type 'string
  :group 'fixed-font)

(defcustom fixed-font-default-height 100
  "글꼴의 기본 크기를 지정한다."
  :type 'int
  :group 'fixed-font)

(defvar fixed-font-current-height fixed-font-default-height)

(defconst fixed-font-rescale-list ())

(add-to-list 'fixed-font-rescale-list
 '(("Default" . "Default")
   ((70  . 1.20) (80  . 1.30) (90  . 1.25) (100 . 1.20) (110 . 1.20)
    (120 . 1.20) (130 . 1.25) (140 . 1.22) (150 . 1.20) (160 . 1.20)
    (170 . 1.20) (180 . 1.20) (190 . 1.22) (200 . 1.20) (210 . 1.20))))

(add-to-list 'fixed-font-rescale-list
 '(("NanumGothicCoding" . "Anonymous Pro")
   ((70  . 1.08) (80  . 1.30) (90  . 1.08) (100 . 1.08) (110 . 1.10)
    (120 . 1.10) (130 . 1.08) (140 . 1.08) (150 . 1.08) (160 . 1.08)
    (170 . 1.12) (180 . 1.12) (190 . 1.12) (200 . 1.12) (210 . 1.12))))

(add-to-list 'fixed-font-rescale-list
 '(("NanumGothicCoding" . "Inconsolata")
   ((70  . 1.00) (80  . 1.30) (90  . 1.00) (100 . 1.00) (110 . 1.00)
    (120 . 1.00) (130 . 1.00) (140 . 1.00) (150 . 1.00) (160 . 1.00)
    (170 . 1.00) (180 . 1.00) (190 . 1.00) (200 . 1.00) (210 . 1.00))))

(add-to-list 'fixed-font-rescale-list
 '(("NanumGothicCoding" . "Ubuntu Mono")
   ((70  . 1.00) (80  . 1.30) (90  . 1.00) (100 . 1.00) (110 . 1.00)
    (120 . 1.00) (130 . 1.00) (140 . 1.00) (150 . 1.00) (160 . 1.00)
    (170 . 1.00) (180 . 1.00) (190 . 1.00) (200 . 1.00) (210 . 1.00))))

(add-to-list 'fixed-font-rescale-list
 '(("NanumGothicCoding" . "B612 Mono")
   ((70  . 1.32) (80  . 1.30) (90  . 1.34) (100 . 1.32) (110 . 1.30)
    (120 . 1.34) (130 . 1.32) (140 . 1.32) (150 . 1.34) (160 . 1.32)
    (170 . 1.32) (180 . 1.30) (190 . 1.32) (200 . 1.32) (210 . 1.32))))

(add-to-list 'fixed-font-rescale-list
 '(("NanumGothicCoding" . "Space Mono")
   ((70  . 1.26) (80  . 1.30) (90  . 1.26) (100 . 1.26) (110 . 1.26)
    (120 . 1.26) (130 . 1.26) (140 . 1.22) (150 . 1.22) (160 . 1.26)
    (170 . 1.26) (180 . 1.24) (190 . 1.22) (200 . 1.22) (210 . 1.24))))

(add-to-list 'fixed-font-rescale-list
 '(("NanumGothicCoding" . "VT323")
   ((70  . 0.84) (80  . 1.30) (90  . 0.84) (100 . 0.84) (110 . 0.84)
    (120 . 0.84) (130 . 0.84) (140 . 0.84) (150 . 0.84) (160 . 0.84)
    (170 . 0.84) (180 . 0.80) (190 . 0.80) (200 . 0.80) (210 . 0.82))))

(defun fixed-font--search (hangul ascii)
  "한글(HANGUL)과 영문(ASCII) 글꼴의 설정을 찾아 반환한다."
  (seq-find
   (lambda (val)
     (and (string= (car (car val)) hangul)
	  (string= (cdr (car val)) ascii)))
	  fixed-font-rescale-list))

(defun fixed-font-search ()
  "한글과 영문 글꼴의 설정을 찾아 반환한다.  만약 설정에 없는 경우 기본값으로 반환한다."
  (let ((font-config (fixed-font--search fixed-font-hangul-font fixed-font-ascii-font)))
    (if (eq font-config nil)
	(fixed-font--search "Default" "Default")
      font-config)))
    
(defun fixed-font--min-height ()
  "글꼴의 최소 크기를 반환한다."
  (let* ((font-config (fixed-font-search))
	 (rescale (car (cdr font-config))))
    (seq-min
     (seq-map (lambda (pair) (car pair)) rescale))))

(defun fixed-font--max-height ()
  "글꼴의 최대 크기를 반환한다."
  (let* ((font-config (fixed-font-search))
	 (rescale (car (cdr font-config))))
    (seq-max
     (seq-map (lambda (pair) (car pair)) rescale))))

(defun fixed-font--rescale (height)
  "영문 글꼴의 크기(HEIGHT)에 해당하는 한글 글꼴의 비율을 반환한다."
  (let ((rescales (fixed-font-search)))
    (cdr (seq-find (lambda (pair) (= (car pair) height)) (car (cdr rescales))))))

(defun fixed-font--set-height (height)
  "영문 글꼴을 지정한 크기(HEIGHT)로 설정하고, 한글 글꼴은 해당하는 비율로 지정한다."
  (let ((min-height (fixed-font--min-height))
	(max-height (fixed-font--max-height))
	(rescale (fixed-font--rescale height)))
    (when (< height min-height) (error "글꼴의 설정이 %d 보다 작을 수 없습니다" min-height))
    (when (> height max-height) (error "글꼴의 설정이 %d 보다 클 수 없습니다" max-height))
    (message "한글글꼴: %s, 영문글꼴: %s, 글꼴크기: %d, 비율: %0.2f" fixed-font-hangul-font fixed-font-ascii-font height rescale)
    (set-fontset-font t 'hangul (font-spec :family fixed-font-hangul-font))
    (setq face-font-rescale-alist `((,fixed-font-hangul-font . ,rescale)))
    (set-face-attribute 'default nil :family fixed-font-ascii-font :height height)))

;;;###autoload
(defun fixed-font-default ()
  "글꼴의 크기를 fixed-font-default-height로 설정한다."
  (interactive)
  (fixed-font--set-height fixed-font-default-height)
  (setq fixed-font-current-height fixed-font-default-height))


;;;###autoload
(defun fixed-font-increase ()
  "글꼴의 크기를 한단계(10) 크게 설정한다."
  (interactive)
  (let ((new-height (+ fixed-font-current-height 10)))
    (fixed-font--set-height new-height)
    (setq fixed-font-current-height new-height)))

;;;###autoload
(defun fixed-font-decrease ()
  "글꼴의 크기를 한단계(10) 작게 설정한다."
  (interactive)
  (let ((new-height (- fixed-font-current-height 10)))
    (fixed-font--set-height new-height)
    (setq fixed-font-current-height new-height)))

(provide 'fixed-font)
;;; fixed-font.el ends here

;;; fixed-font.el --- 한글 고정폭 글꼴 설정 -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Inforgra

;; Author: Kukjin Kang <kj.kang@daum.net>
;; Keywords: font
;; Version: 0.1.0

;;; Commentary:
;;
;; https://github.com/kj-kang/fixed-font/README.md 를 참고한다.

;;; Code:

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

## 개요

글꼴의 경우 글자폭의 형태에 따라 가변폭, 고정폭으로 구분한다. 가변폭의
경우 "W" 와 "I" 의 가로폭(width)이 다르며, 고정폭은 "I" 의 폭을 "W" 에
맞추어 개발한 글꼴이다. 일반적으로 가변폭 형태의 글꼴이 읽이게
편안하고 가독성이 높다. 그러나 숫자, 기호, 띄어쓰기 등을 명확하게
표현하기가 어렵다.

같은 고정폭이라 하더라도 영문과 한글 글꼴을 같이 사용하는 경우 폭이
달라질 수 있다. 아래와 같이 텍스트로 테이블을 만드는 경우 그 차이를
확인할 수 있다. "|" 폭이 같으면 고정폭 글꼴을 사용한다고 생각하면
된다.

```
| 한글 | 테스트 |
| ABCD | abcdef |
```

이 패키지는 Eamcs에서 영문과 한글 글꼴을 같은 고정폭으로 사용할 수
있도록 한다. 한글 글꼴은
[D2Coding](https://github.com/naver/d2codingfont) 을 기본으로 하고
영문은 다음과 같은 글꼴을 테스트하였다.

- [Envy Code R](https://damieng.com/blog/2008/05/26/envy-code-r-preview-7-coding-font-released)
- [DejaVu Sans Mono](https://dejavu-fonts.github.io/)
- [Droid Sans Mono](https://www.droidfonts.com/info/droid-sans-mono-fonts/)
- [Fira Mono](https://mozilla.github.io/Fira/)
- [Hack](https://sourcefoundry.org/hack/)
- Menlo
- Monaco
- [Source Code Pro](https://github.com/adobe-fonts/source-code-pro)

## 설치

`fixed-font.el` 파일을 `~/.emacs.d/site-lisp` 디렉토리에
다운로드한다. 아래와 같이 `.emacs` (or `init.el`) 파일에 추가한다.

```lisp
(use-package fixed-font
  :load-path "~/.emacs.d/site-lisp/fixed-font"
  :config
  (fixed-font-set-height-default))
```

## 설정

기본 영문, 한글 글꼴과 크기는 다음과 같이 지정한다.

```lisp
;; 영문 글꼴을 지정한다.
(setq fixed-font-ascii-font "Monaco")

;; 한글 글꼴을 지정한다.
(setq fixed-font-non-ascii-font "D2Coding")

;; 기본크기를 지정한다.
(setq fixed-font-default-height 130)
```

`use-pacakge` 내의 `bind` 설정을 통해 아래와 같이 단축키를 지정할 수
있다.

```
(use-package fixed-font
  :load-path "~/.emacs.d/site-lisp/fixed-font"
  :bind
  ;; 기본 글꼴, 크기로 변경한다.
  ("s-0" . fixed-font-set-height-default)

  ;; 글꼴을 한단계 증가시킨다.
  ("s-=" . fixed-font-increase-height)

  ;; 글꼴을 한단계 감소시킨다.
  ("s--" . fixed-font-decrease-height))
```

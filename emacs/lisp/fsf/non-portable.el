;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2011-06-28 16:43:07 attila@stalphonsos.com>
;;;
;;; Non-portable stuff, per EMACS variant.  Pulled in by generic-setup.el
;;; as a part of my generic .emacs fu.
;;;
(require 'info)

(fset 'attila-perl-podly
   [?\M-f ?\C-f ?\C-  ?\C-s ?{ ?\C-b ?\C-b ?\M-w ?\C-a ?\C-o ?= ?p ?o ?d return return ?= ?o ?v ?e ?r ?  ?4 return return ?= ?i ?t ?e ?m ?  ?\C-y ?  ?. ?. ?. return return ?= ?b ?a ?c ?k return return ?= ?c ?u ?t return ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-e])

;;; I have my own Info dir

(setq Info-directory-list
      (append (list (expand-file-name "~/emacs/info/gnu"))
              Info-directory-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Useful character codes in GNU Emacs ...
;;
;; 2209 => ¡ ;; 2210 => ¢ ;; 2211 => £ ;; 2212 => ¤ ;; 2213 => ¥ ;; 2214 => ¦
;; 2215 => § ;; 2216 => ¨ ;; 2217 => © ;; 2218 => ª ;; 2219 => « ;; 2220 => ¬
;; 2221 => ­ ;; 2222 => ® ;; 2223 => ¯ ;; 2224 => ° ;; 2225 => ± ;; 2226 => ²
;; 2227 => ³ ;; 2228 => ´ ;; 2229 => µ ;; 2230 => ¶ ;; 2231 => · ;; 2232 => ¸
;; 2233 => ¹ ;; 2234 => º ;; 2235 => » ;; 2236 => ¼ ;; 2237 => ½ ;; 2238 => ¾
;; 2239 => ¿ ;; 2240 => À ;; 2241 => Á ;; 2242 => Â ;; 2243 => Ã ;; 2244 => Ä
;; 2245 => Å ;; 2246 => Æ ;; 2247 => Ç ;; 2248 => È ;; 2249 => É ;; 2250 => Ê
;; 2251 => Ë ;; 2252 => Ì ;; 2253 => Í ;; 2254 => Î ;; 2255 => Ï ;; 2256 => Ð
;; 2257 => Ñ ;; 2258 => Ò ;; 2259 => Ó ;; 2260 => Ô ;; 2261 => Õ ;; 2262 => Ö
;; 2263 => × ;; 2264 => Ø ;; 2265 => Ù ;; 2266 => Ú ;; 2267 => Û ;; 2268 => Ü
;; 2269 => Ý ;; 2270 => Þ ;; 2271 => ß ;; 2272 => à ;; 2273 => á ;; 2274 => â
;; 2275 => ã ;; 2276 => ä ;; 2277 => å ;; 2278 => æ ;; 2279 => ç ;; 2280 => è
;; 2281 => é ;; 2282 => ê ;; 2283 => ë ;; 2284 => ì ;; 2285 => í ;; 2286 => î
;; 2287 => ï ;; 2288 => ð ;; 2289 => ñ ;; 2290 => ò ;; 2291 => ó ;; 2292 => ô
;; 2293 => õ ;; 2294 => ö ;; 2295 => ÷ ;; 2296 => ø ;; 2297 => ù ;; 2298 => ú
;; 2299 => û ;; 2300 => ü ;; 2301 => ý ;; 2302 => þ ;; 2303 => ÿ
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... and the keybindings that love them.
;;
;; These are all for typing in Español.  Using C-c seemed natural.  Maybe
;; there's an easier way to do this, but I don't know it.
;;
;; First, some things to call from keybindings
;;
(defun attila-insert-¿ () (interactive) (insert 2239))
(defun attila-insert-¡ () (interactive) (insert 2209))
(defun attila-insert-á () (interactive) (insert 2273))
(defun attila-insert-Á () (interactive) (insert 2241))
(defun attila-insert-é () (interactive) (insert 2281))
(defun attila-insert-É () (interactive) (insert 2249))
(defun attila-insert-í () (interactive) (insert 2285))
(defun attila-insert-Í () (interactive) (insert 2253))
(defun attila-insert-ó () (interactive) (insert 2291))
(defun attila-insert-Ó () (interactive) (insert 2259))
(defun attila-insert-ú () (interactive) (insert 2298))
(defun attila-insert-Ú () (interactive) (insert 2266))
(defun attila-insert-ñ () (interactive) (insert 2289))
(defun attila-insert-Ñ () (interactive) (insert 2257))
;;
;; Then, some keybindings to call them
;;
;(global-set-key "\C-c?" 'attila-insert-¿)
;(global-set-key "\C-c!" 'attila-insert-¡)
;(global-set-key "\C-ca" 'attila-insert-á)
;(global-set-key "\C-cA" 'attila-insert-Á)
;(global-set-key "\C-ce" 'attila-insert-é)
;(global-set-key "\C-cE" 'attila-insert-É)
;(global-set-key "\C-ci" 'attila-insert-í)
;(global-set-key "\C-cI" 'attila-insert-Í)
;(global-set-key "\C-co" 'attila-insert-ó)
;(global-set-key "\C-cO" 'attila-insert-Ó)
;(global-set-key "\C-cu" 'attila-insert-ú)
;(global-set-key "\C-cU" 'attila-insert-Ú)
;(global-set-key "\C-cn" 'attila-insert-ñ)
;(global-set-key "\C-cN" 'attila-insert-Ñ)

(load-path-append "/usr/share/emacs/site-lisp")
(load-path-append "/usr/share/emacs/site-lisp/slime")

;(require 'jit-lock)
;(setq font-lock-support-mode 'jit-lock-mode)
;;(setq font-lock-support-mode 'fast-lock-mode)
(provide 'non-portable)

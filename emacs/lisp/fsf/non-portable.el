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
;; 2209 => � ;; 2210 => � ;; 2211 => � ;; 2212 => � ;; 2213 => � ;; 2214 => �
;; 2215 => � ;; 2216 => � ;; 2217 => � ;; 2218 => � ;; 2219 => � ;; 2220 => �
;; 2221 => � ;; 2222 => � ;; 2223 => � ;; 2224 => � ;; 2225 => � ;; 2226 => �
;; 2227 => � ;; 2228 => � ;; 2229 => � ;; 2230 => � ;; 2231 => � ;; 2232 => �
;; 2233 => � ;; 2234 => � ;; 2235 => � ;; 2236 => � ;; 2237 => � ;; 2238 => �
;; 2239 => � ;; 2240 => � ;; 2241 => � ;; 2242 => � ;; 2243 => � ;; 2244 => �
;; 2245 => � ;; 2246 => � ;; 2247 => � ;; 2248 => � ;; 2249 => � ;; 2250 => �
;; 2251 => � ;; 2252 => � ;; 2253 => � ;; 2254 => � ;; 2255 => � ;; 2256 => �
;; 2257 => � ;; 2258 => � ;; 2259 => � ;; 2260 => � ;; 2261 => � ;; 2262 => �
;; 2263 => � ;; 2264 => � ;; 2265 => � ;; 2266 => � ;; 2267 => � ;; 2268 => �
;; 2269 => � ;; 2270 => � ;; 2271 => � ;; 2272 => � ;; 2273 => � ;; 2274 => �
;; 2275 => � ;; 2276 => � ;; 2277 => � ;; 2278 => � ;; 2279 => � ;; 2280 => �
;; 2281 => � ;; 2282 => � ;; 2283 => � ;; 2284 => � ;; 2285 => � ;; 2286 => �
;; 2287 => � ;; 2288 => � ;; 2289 => � ;; 2290 => � ;; 2291 => � ;; 2292 => �
;; 2293 => � ;; 2294 => � ;; 2295 => � ;; 2296 => � ;; 2297 => � ;; 2298 => �
;; 2299 => � ;; 2300 => � ;; 2301 => � ;; 2302 => � ;; 2303 => �
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ... and the keybindings that love them.
;;
;; These are all for typing in Espa�ol.  Using C-c seemed natural.  Maybe
;; there's an easier way to do this, but I don't know it.
;;
;; First, some things to call from keybindings
;;
(defun attila-insert-� () (interactive) (insert 2239))
(defun attila-insert-� () (interactive) (insert 2209))
(defun attila-insert-� () (interactive) (insert 2273))
(defun attila-insert-� () (interactive) (insert 2241))
(defun attila-insert-� () (interactive) (insert 2281))
(defun attila-insert-� () (interactive) (insert 2249))
(defun attila-insert-� () (interactive) (insert 2285))
(defun attila-insert-� () (interactive) (insert 2253))
(defun attila-insert-� () (interactive) (insert 2291))
(defun attila-insert-� () (interactive) (insert 2259))
(defun attila-insert-� () (interactive) (insert 2298))
(defun attila-insert-� () (interactive) (insert 2266))
(defun attila-insert-� () (interactive) (insert 2289))
(defun attila-insert-� () (interactive) (insert 2257))
;;
;; Then, some keybindings to call them
;;
;(global-set-key "\C-c?" 'attila-insert-�)
;(global-set-key "\C-c!" 'attila-insert-�)
;(global-set-key "\C-ca" 'attila-insert-�)
;(global-set-key "\C-cA" 'attila-insert-�)
;(global-set-key "\C-ce" 'attila-insert-�)
;(global-set-key "\C-cE" 'attila-insert-�)
;(global-set-key "\C-ci" 'attila-insert-�)
;(global-set-key "\C-cI" 'attila-insert-�)
;(global-set-key "\C-co" 'attila-insert-�)
;(global-set-key "\C-cO" 'attila-insert-�)
;(global-set-key "\C-cu" 'attila-insert-�)
;(global-set-key "\C-cU" 'attila-insert-�)
;(global-set-key "\C-cn" 'attila-insert-�)
;(global-set-key "\C-cN" 'attila-insert-�)

(load-path-append "/usr/share/emacs/site-lisp")
(load-path-append "/usr/share/emacs/site-lisp/slime")

;(require 'jit-lock)
;(setq font-lock-support-mode 'jit-lock-mode)
;;(setq font-lock-support-mode 'fast-lock-mode)
(provide 'non-portable)

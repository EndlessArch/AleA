# AleA
아래아

## System grammar
```lisp
; basic function call
(void-f)
(unary-f arg)
(binary-f arg1 arg2)

; with gydr & gydi
(gydr-f :gydr gydi)
(gydrs-f
  :gydr1 gydi1
  :gydr2 gydi2 )

; optional parameters
; equivalant to:
; (printf: &!cpt Int
;   v1: (...) v2: (...) v3: (...) ...)
; (any: &cpt) means
; following arguments of function `print`
; should be compile-time type
; And of course, AleA can be dependent type
(printf: &!cpt Int
  { (v: &cpt ?) })

; Types, which are used in parameter
; by caller/callee
var: type
arr: [type]
foo: &cpt type    ; == compile-time type
bar: [|cpt type]  ; == constexpr array type
baz: &!cpt type   ; == io type (maybe unoptimized)

; Type signature
(__df
  :from (?:[|cpt any])
  :to (?:[|cpt any])
  { ; optional gydr/gydis
    :in-scope-of (? : (? :return-type= Bool))
    ; or
    :in-scope-of (? : ?.. -> Bool)
    :system (? : (? : ))
   })
```

## Simple example/application
```lisp
(__df
  :from (
    (a: idf)
    ":="
    (b: expr))
  :to (
    a "=" b
  )
  :in-scope-of ; globally
    (True) ; or
    (:type expr) ; or
    (:apply-by-keyword `defun`) ; or
  :system (

  )
)
```
# AleA
아래아

## 철학 ?
- 동작은 짧게
- 명시는 엄밀하게
- 기교는 최대로

## System grammar
```lisp
@cnt => Countable ( is array/list? )
@cpt => Compile-Time constraint
        the codes are only generated just once at compile time
@len => the type, kind of length
@udf => Undefined yet
@foreign(tps) => Explicit foreign type

; optional parameters
; equivalant to:
; (defun printf : (v1: &cpt v1_t) -> (v2: &cpt v2_t) -> (vN: &cpt vN_t) -> &IO Int
;   ...  )
; (? : &cpt t) means
; the type of following arguments of function
; should be compile-time type
; And of course, AleA can be dependent type

(type= IO !cpt)

; (defun unfold_t : (argName : String) (argTypes : [@type ?]) -> [(@arg, @type)]
;   for (i, type) of (zipN argTypes) (
;     ((@arg argName#(+ i 1)), (@type type))
;   ) )

;;;

(defun zip
  : forall a b (l, (== len (a' : @udf [?])
  . (a' : [a]) (b' : @l [b]) -> @l [(a, b)]
  =>
  [ for i of l where (a'::i, b'::i) ]. )

; binary function
(defun (<:>)
  : forall a b (l, (== len (as : @udf [?])) )
  . (as : [a]) (bs : @l [b]) -> ([(a, b)] : @l ?) =>
  zip as bs. )

(defun printf : (s : String) (vs : [ a1.. ] <:> { _ : @show ? }) -> @IO Int
  (\(a, b). a < 0 ? a : puts b) : @IO Int / sprintf s vs. )

; int sprintf(const char *, const char *, ...)
(defun sprintf
  : forall
  . (s : String) (vs : { v : @show ? }) -> (int, String) =>
  (s' : @CC(const char *) ) = ""
  flag = (sprintf : @CC(const char *, const char *, ... -> int) )
          / s' (s : @CC(const char *)) (vs : @CC(v))
  (flag, s'). )

; Function
;   (Defun "puts"
;     (Forall [])
;     (Type [String, Bool])
;     (Args [ (Idf "s") ])
;     [ (NewVar [] (Idf "s\'") (@CC(const char *)) (Idf "s"))
;       (NewVar (@CC((Idf int))) (Call (Function (Defun "puts" () () ()))))] )
(defun puts : forall . (s : String) -> Bool =>
  s' = (s : @CC(const char *))
  (r : @CC(int)) = (puts : @CC(const char * -> int)) s'
  r >= 0.
)

; ; Type signature
; (__df
;   :from (?:[|cpt any])
;   :to (?:[|cpt any])
;   { ; optional gydr/gydis
;     :in-scope-of (? : ?.. -> Bool)
;     :system (? : (? : ))
;    })
```
<!-- 
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
) -->
```
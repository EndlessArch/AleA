# Change log for the `llvm-tf` package

## 9.2

* custom `Ptr` type:
  We leave the original `Ptr` type for data in `Storable` compatible format,
  and use `LLVM.Ptr` for data in LLVM layout.

* `instance Storable Vector`:
  Allows non-primitive elements and interleaves them.

* `instance Marshal Vector`:
  Should now be really compatible with LLVM.
  Formerly, it was wrong on big-endian systems
  and vectors of Bool, WordN, IntN.
  The correct implementation required a new class for storing vectors.

* `Ret` class: turned from multi-parameter type class
  to single parameter type class with type function `Result`.
  You may replace `Ret a r` by `Ret a, Result a ~ r` in your code,
  which may enable further simplifications.

* `CallArgs f g r` -> `CallArgs r f g`,
  `CallerFunction f r` -> `CallerFunction r f`

* `ArithFunction`, `ToArithFunction`:
  Replaced functional dependencies by type functions.

* `ArithFunction`: split off `Return`

## 9.0

* `Instructions.bitcastElements`:
  Use `Guided.bitcast Guided.vector` instead.

* `Core.Guided`: new module for instructions on both scalars and vectors

* fixed bug: `cmp` on `IntN` did an unsigned comparison

* `Vector`: instance `QuickCheck.Arbitrary`

## 3.1.2

* `Instructions`: setters for FastMath flags

## 3.1.0.1

* `addFunctionMapping` checks for functions
  that are eliminated by optimization passes.
  This fixes a crash when working with optimizations and call-back functions.

## 3.1

* `ExecutionEngine` is now managed by a `ForeignPtr` with a finalizer.
  That is, you must keep the `ExecutionEngine` alive
  as long as you call compiled functions.

  `FreePointers` and `getFreePointers` are gone.

## 3.0.3

* `constVector`, `constArray`, `vector` do no longer cycle the vector
  Instead they check for the appropriate static length.

* `FFI.constVector`, `FFI.constArray` must be in IO
  in order to proper sequence actions in `Core.Util.constVector`, `Core.Util.constArray`.
  Currently, in `Util.constVector` it is possible that `FFI.constArray`
  is called too late and thus operates on a released pointer.

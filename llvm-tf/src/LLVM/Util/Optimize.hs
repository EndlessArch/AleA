module LLVM.Util.Optimize(optimizeModule) where

import LLVM.Core.Util (Module, withModule, getObjList)

import qualified LLVM.FFI.Transforms.PassManagerBuilder as PMB
import qualified LLVM.FFI.Core as FFI
import LLVM.FFI.Transforms.Scalar (addVerifierPass)

import Control.Exception (bracket, bracket_)


{- |
Result tells whether the module was modified by any of the passes.

It is very important that you set target triple and target data layout
before optimizing.
Otherwise the optimizer will make wrong assumptions
and e.g. corrupt your record offsets.
See e.g. example/Array for how this can be achieved.

In the future I might enforce via types
that you set target parameters before optimization.
-}
optimizeModule :: Int -> Module -> IO Bool
optimizeModule optLevel mdl =
    withModule mdl $ \ m ->
    {-
    Core.Util.createPassManager would provide a finalizer for us,
    but I think it is better here to immediately dispose the manager
    when we need it no longer.
    -}
    bracket FFI.createPassManager FFI.disposePassManager $ \mpasses ->
    bracket (FFI.createFunctionPassManagerForModule m)
            FFI.disposePassManager $ \fpasses -> do
        addVerifierPass mpasses

        bracket PMB.create PMB.dispose $ \passBuilder -> do
            PMB.setOptLevel passBuilder $ fromIntegral optLevel
            {-
            PMB.setSizeLevel passBuilder 1
            PMB.setDisableUnitAtATime passBuilder false
            PMB.setDisableUnrollLoops passBuilder (optLevel <= 1)
            PMB.setDisableSimplifyLibCalls passBuilder false
            PMB.setHaveExceptions passBuilder true
            PMB.setInliningPass passBuilder true
            -}

            PMB.populateFunctionPassManager passBuilder fpasses
            PMB.populateModulePassManager passBuilder mpasses

        fchanged <-
            bracket_
                (FFI.initializeFunctionPassManager fpasses)
                (FFI.finalizeFunctionPassManager fpasses)
                (mapM (FFI.runFunctionPassManager fpasses) =<<
                 getObjList withModule
                    FFI.getFirstFunction FFI.getNextFunction mdl)
        mchanged <- FFI.runPassManager mpasses m
        return $ any FFI.deconsBool $ mchanged : fchanged

{-
ToDo:
Function that adds passes according to a list of opt-options.
This would simplify to get consistent behaviour between opt and optimizeModule.

-adce                      addAggressiveDCEPass
-deadargelim               addDeadArgEliminationPass
-deadtypeelim              addDeadTypeEliminationPass
-dse                       addDeadStoreEliminationPass
-functionattrs             addFunctionAttrsPass
-globalopt                 addGlobalOptimizerPass
-indvars                   addIndVarSimplifyPass
-instcombine               addInstructionCombiningPass
-ipsccp                    addIPSCCPPass
-jump-threading            addJumpThreadingPass
-licm                      addLICMPass
-loop-deletion             addLoopDeletionPass
-loop-rotate               addLoopRotatePass
-memcpyopt                 addMemCpyOptPass
-prune-eh                  addPruneEHPass
-reassociate               addReassociatePass
-scalarrepl                addScalarReplAggregatesPass
-sccp                      addSCCPPass
-simplifycfg               addCFGSimplificationPass
-simplify-libcalls         addSimplifyLibCallsPass
-strip-dead-prototypes     addStripDeadPrototypesPass
-tailcallelim              addTailCallEliminationPass
-verify                    addVerifierPass
-}

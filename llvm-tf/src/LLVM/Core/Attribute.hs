module LLVM.Core.Attribute (
    zeroext,
    signext,
    inreg,
    byval,
    sret,
    align,
    noalias,
    nocapture,
    nest,
    returned,
    nonnull,
    dereferenceable,
    dereferenceableOrNull,
    swiftself,
    swifterror,
    immarg,
    alignstack,
    allocsize,
    alwaysinline,
    builtin,
    cold,
    convergent,
    inaccessiblememonly,
    inaccessiblememOrArgmemonly,
    inlinehint,
    jumptable,
    minsize,
    naked,
    noJumpTables,
    nobuiltin,
    noduplicate,
    nofree,
    noimplicitfloat,
    noinline,
    nonlazybind,
    noredzone,
    indirectTlsSegRefs,
    noreturn,
    norecurse,
    willreturn,
    nosync,
    nounwind,
    nullPointerIsValid,
    optforfuzzing,
    optnone,
    optsize,
    patchableFunction,
    probeStack,
    readnone,
    readonly,
    stackProbeSize,
    noStackArgProbe,
    writeonly,
    argmemonly,
    returnsTwice,
    safestack,
    sanitizeAddress,
    sanitizeMemory,
    sanitizeThread,
    sanitizeHwaddress,
    sanitizeMemtag,
    speculativeLoadHardening,
    speculatable,
    ssp,
    sspreq,
    sspstrong,
    strictfp,
    uwtable,
    nocfCheck,
    shadowcallstack,
    ) where

import LLVM.Core.CodeGen (Attribute(Attribute))

import qualified LLVM.FFI.Core.Attribute as Attr

import Data.Word (Word64)


simple :: Attr.Name -> Attribute
simple name = Attribute name 0

withParam :: Attr.Name -> Word64 -> Attribute
withParam = Attribute

-- * Parameter attributes

zeroext :: Attribute
zeroext = simple Attr.zeroext

signext :: Attribute
signext = simple Attr.signext

inreg :: Attribute
inreg = simple Attr.inreg

byval :: Attribute
byval = simple Attr.byval

sret :: Attribute
sret = simple Attr.sret

align :: Word64 -> Attribute
align = withParam Attr.align

noalias :: Attribute
noalias = simple Attr.noalias

nocapture :: Attribute
nocapture = simple Attr.nocapture

nest :: Attribute
nest = simple Attr.nest

returned :: Attribute
returned = simple Attr.returned

nonnull :: Attribute
nonnull = simple Attr.nonnull

dereferenceable :: Word64 -> Attribute
dereferenceable = withParam Attr.dereferenceable

dereferenceableOrNull :: Word64 -> Attribute
dereferenceableOrNull = withParam Attr.dereferenceableOrNull

swiftself :: Attribute
swiftself = simple Attr.swiftself

swifterror :: Attribute
swifterror = simple Attr.swifterror

immarg :: Attribute
immarg = simple Attr.immarg


-- * Function attributes

alignstack :: Word64 -> Attribute
alignstack = withParam Attr.alignstack

allocsize :: Attribute
allocsize = simple Attr.allocsize

alwaysinline :: Attribute
alwaysinline = simple Attr.alwaysinline

builtin :: Attribute
builtin = simple Attr.builtin

cold :: Attribute
cold = simple Attr.cold

convergent :: Attribute
convergent = simple Attr.convergent

inaccessiblememonly :: Attribute
inaccessiblememonly = simple Attr.inaccessiblememonly

inaccessiblememOrArgmemonly :: Attribute
inaccessiblememOrArgmemonly = simple Attr.inaccessiblememOrArgmemonly

inlinehint :: Attribute
inlinehint = simple Attr.inlinehint

jumptable :: Attribute
jumptable = simple Attr.jumptable

minsize :: Attribute
minsize = simple Attr.minsize

naked :: Attribute
naked = simple Attr.naked

noJumpTables :: Attribute
noJumpTables = simple Attr.noJumpTables

nobuiltin :: Attribute
nobuiltin = simple Attr.nobuiltin

noduplicate :: Attribute
noduplicate = simple Attr.noduplicate

nofree :: Attribute
nofree = simple Attr.nofree

noimplicitfloat :: Attribute
noimplicitfloat = simple Attr.noimplicitfloat

noinline :: Attribute
noinline = simple Attr.noinline

nonlazybind :: Attribute
nonlazybind = simple Attr.nonlazybind

noredzone :: Attribute
noredzone = simple Attr.noredzone

indirectTlsSegRefs :: Attribute
indirectTlsSegRefs = simple Attr.indirectTlsSegRefs

noreturn :: Attribute
noreturn = simple Attr.noreturn

norecurse :: Attribute
norecurse = simple Attr.norecurse

willreturn :: Attribute
willreturn = simple Attr.willreturn

nosync :: Attribute
nosync = simple Attr.nosync

nounwind :: Attribute
nounwind = simple Attr.nounwind

nullPointerIsValid :: Attribute
nullPointerIsValid = simple Attr.nullPointerIsValid

optforfuzzing :: Attribute
optforfuzzing = simple Attr.optforfuzzing

optnone :: Attribute
optnone = simple Attr.optnone

optsize :: Attribute
optsize = simple Attr.optsize

patchableFunction :: Attribute
patchableFunction = simple Attr.patchableFunction

probeStack :: Attribute
probeStack = simple Attr.probeStack

readnone :: Attribute
readnone = simple Attr.readnone

readonly :: Attribute
readonly = simple Attr.readonly

stackProbeSize :: Attribute
stackProbeSize = simple Attr.stackProbeSize

noStackArgProbe :: Attribute
noStackArgProbe = simple Attr.noStackArgProbe

writeonly :: Attribute
writeonly = simple Attr.writeonly

argmemonly :: Attribute
argmemonly = simple Attr.argmemonly

returnsTwice :: Attribute
returnsTwice = simple Attr.returnsTwice

safestack :: Attribute
safestack = simple Attr.safestack

sanitizeAddress :: Attribute
sanitizeAddress = simple Attr.sanitizeAddress

sanitizeMemory :: Attribute
sanitizeMemory = simple Attr.sanitizeMemory

sanitizeThread :: Attribute
sanitizeThread = simple Attr.sanitizeThread

sanitizeHwaddress :: Attribute
sanitizeHwaddress = simple Attr.sanitizeHwaddress

sanitizeMemtag :: Attribute
sanitizeMemtag = simple Attr.sanitizeMemtag

speculativeLoadHardening :: Attribute
speculativeLoadHardening = simple Attr.speculativeLoadHardening

speculatable :: Attribute
speculatable = simple Attr.speculatable

ssp :: Attribute
ssp = simple Attr.ssp

sspreq :: Attribute
sspreq = simple Attr.sspreq

sspstrong :: Attribute
sspstrong = simple Attr.sspstrong

strictfp :: Attribute
strictfp = simple Attr.strictfp

uwtable :: Attribute
uwtable = simple Attr.uwtable

nocfCheck :: Attribute
nocfCheck = simple Attr.nocfCheck

shadowcallstack :: Attribute
shadowcallstack = simple Attr.shadowcallstack

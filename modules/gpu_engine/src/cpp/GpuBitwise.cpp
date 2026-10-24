//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Bitwise operations on integer gpuArray. int32/uint32 use the native i32/u32
// kernels; int8/16 use the f32 kernels via an i32 conversion. Non-integer
// inputs are rejected.
//=============================================================================
#include "GpuKernels.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
gpuBitLogic(GpuArrayObject* a, GpuArrayObject* b, const std::string& op, const char* wgslOp);
//=============================================================================
ArrayOf
GpuBitand(GpuArrayObject* a, GpuArrayObject* b)
{
    return gpuBitLogic(a, b, "bitand", "&");
}
//=============================================================================
ArrayOf
GpuBitor(GpuArrayObject* a, GpuArrayObject* b)
{
    return gpuBitLogic(a, b, "bitor", "|");
}
//=============================================================================
ArrayOf
GpuBitxor(GpuArrayObject* a, GpuArrayObject* b)
{
    return gpuBitLogic(a, b, "bitxor", "^");
}
//=============================================================================
ArrayOf
GpuBitshift(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:GPU:bitwiseRequiresInteger",
            L"Bitwise operations require an integer gpuArray.");
    }
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "bitshift");
    }
    // int8/16 bitshift truncates to the class width, which conflicts with the
    // saturating f32 gather; only int32/uint32 are supported.
    if (!GpuIntegerResultType(a, a).empty()) {
        Error(L"Nelson:bitshift:unsupportedType",
            L"bitshift on int8/int16 gpuArray is not supported; use int32/uint32.");
    }
    Error(L"Nelson:GPU:bitwiseRequiresInteger", L"Bitwise operations require an integer gpuArray.");
    return {};
}
//=============================================================================
// static helper functions
//=============================================================================
static ArrayOf
gpuBitLogic(GpuArrayObject* a, GpuArrayObject* b, const std::string& op, const char* wgslOp)
{
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:GPU:bitwiseRequiresInteger",
            L"Bitwise operations require an integer gpuArray.");
    }
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, op);
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (intType.empty()) {
        Error(L"Nelson:GPU:bitwiseRequiresInteger",
            L"Bitwise operations require an integer gpuArray.");
    }
    std::string expr = std::string("f32(i32(ai) ") + wgslOp + " i32(bi))";
    return GpuBinaryElementwise(a, b, "gpuArray:" + op + ":" + intType, expr, intType);
}
//=============================================================================
} // namespace Nelson
//=============================================================================

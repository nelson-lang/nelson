//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GpuWebGpu.hpp"
#include "ArrayOf.hpp"
#include "Dimensions.hpp"
#include "HandleGenericObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
// Handle category local to this optional module (kept out of the core
// Types.hpp on purpose so the gpu_engine module stays fully detachable).
#define NLS_HANDLE_GPUARRAY_CATEGORY_STR "gpuArray"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * How the f32 storage buffer is interpreted by the kernels.
 *  - Float32: real values (also int8/16/uint8/16, whose values are exact in
 *    f32 and share the f32 kernels).
 *  - Int32 / Uint32: the buffer holds the raw 32-bit integer bits; kernels
 *    bitcast<i32>/<u32> to operate on them exactly (f32 cannot represent 32-bit
 *    integers). Only the value-interpreting ops need a dedicated i32/u32 path;
 *    byte-moving ops (reshape, transpose, concat, indexing) work unchanged.
 */
enum class GpuStorage
{
    Float32,
    Int32,
    Uint32
};
//=============================================================================
/**
 * Device-resident dense array (WebGPU storage buffer).
 *
 * The buffer stores f32 elements in column-major order, matching the
 * host-side single class layout. className() returns "gpuArray" which
 * routes operators to the @gpuArray overloads.
 */
class NLSGPU_ENGINE_IMPEXP GpuArrayObject : public HandleGenericObject
{
public:
    GpuArrayObject(WGPUBuffer buffer, const Dimensions& dims,
        const std::string& underlyingType = "single", bool complex = false);
    ~GpuArrayObject() override;

    std::string
    className() override
    {
        return NLS_HANDLE_GPUARRAY_CATEGORY_STR;
    }

    void
    onDelete(Evaluator* eval) override;

    WGPUBuffer
    buffer() const
    {
        return m_buffer;
    }
    const Dimensions&
    dimensions() const
    {
        return m_dimensions;
    }
    indexType
    elementCount() const
    {
        return m_dimensions.elementCount();
    }
    /**
     * Underlying device element type as a Nelson class name. The device buffer
     * is always stored as f32; "logical" marks that gather() must
     * produce a logical host array (0/1).
     */
    std::string
    underlyingType() const
    {
        return m_underlyingType;
    }
    bool
    isLogical() const
    {
        return m_underlyingType == "logical";
    }
    GpuStorage
    storageKind() const
    {
        if (m_underlyingType == "int32") {
            return GpuStorage::Int32;
        }
        if (m_underlyingType == "uint32") {
            return GpuStorage::Uint32;
        }
        return GpuStorage::Float32;
    }
    /** True when the buffer holds raw 32-bit integer bits (int32/uint32). */
    bool
    isRawIntStorage() const
    {
        return storageKind() != GpuStorage::Float32;
    }
    /**
     * Complex arrays store interleaved (re, im) f32 pairs, matching the host
     * NLS_SCOMPLEX layout, so the device buffer holds 2 * elementCount floats.
     */
    bool
    isComplex() const
    {
        return m_complex;
    }
    /** Number of f32 slots in the device buffer (2x for complex). */
    indexType
    floatCount() const
    {
        return m_dimensions.elementCount() * (m_complex ? 2 : 1);
    }
    bool
    isValid() const
    {
        return m_buffer != nullptr;
    }

private:
    void
    releaseBuffer();

    WGPUBuffer m_buffer = nullptr;
    Dimensions m_dimensions;
    std::string m_underlyingType;
    bool m_complex = false;
};
//=============================================================================
/**
 * Return the GpuArrayObject wrapped by a scalar gpuArray handle,
 * or nullptr when the value is not a gpuArray.
 */
NLSGPU_ENGINE_IMPEXP GpuArrayObject*
AsGpuArrayObject(const ArrayOf& value);
//=============================================================================
/**
 * Small signed/unsigned integer classes the device supports by reusing the
 * f32 storage and kernels: every representable value fits exactly in f32
 * (|v| < 2^24), so the f32 kernels compute them without loss. Saturation is
 * reproduced by wrapping a result expression in clamp(round(expr), lo, hi).
 *
 * Returns true for "int8"/"uint8"/"int16"/"uint16" and fills hostClass with
 * the matching NelsonType and wgslLo/wgslHi with the saturation bounds as
 * WGSL f32 literals. Returns false for any other underlying type (int32 and
 * wider are intentionally excluded: their products exceed f32 precision and
 * require true i32/u32 kernels).
 */
NLSGPU_ENGINE_IMPEXP bool
GpuIntegerUnderlyingType(const std::string& underlyingType, NelsonType& hostClass,
    std::string& wgslLo, std::string& wgslHi);
//=============================================================================
/**
 * Result integer class of an operation whose operands are a and b (b may be a
 * again for a unary op): matches the host rules, i.e. int op int of the same
 * class stays that class, a class mismatch raises an error, and int op real
 * yields the integer class. Returns "" when neither operand is a small integer
 * gpuArray (the caller then takes the plain f32 path).
 */
NLSGPU_ENGINE_IMPEXP std::string
GpuIntegerResultType(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
/**
 * Wrap an f32 WGSL result expression so it behaves like host integer
 * arithmetic: round half away from zero, then saturate to the class range.
 * Exact integer results (add/sub/mul) are unchanged; only fractional results
 * (e.g. division, mean) are rounded.
 */
NLSGPU_ENGINE_IMPEXP std::string
GpuSaturateExpr(const std::string& expr, const std::string& intType);
//=============================================================================
} // namespace Nelson
//=============================================================================

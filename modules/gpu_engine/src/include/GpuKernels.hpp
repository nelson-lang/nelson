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
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "Dimensions.hpp"
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
#include "GpuElementwiseKernels.hpp"
#include "GpuComplexKernels.hpp"
#include "GpuInteger32Kernels.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Device array of the given shape filled with a constant (0 for zeros, 1 for
// ones); no host transfer.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuFilled(const Dimensions& dims, float value, const std::string& underlyingType = "single");
//=============================================================================
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuEye(const Dimensions& dims, const std::string& underlyingType = "single");
//=============================================================================
// Affine row vector base, base+inc, ..., built on the device (used by colon and
// linspace). When forceLast is true the last element is set exactly to
// lastValue (linspace pins its endpoint).
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAffineSequence(indexType n, float base, float inc, bool forceLast, float lastValue);
//=============================================================================
// Per-element hash PRNG; a global counter advances the stream between calls.
// normal=false gives uniform [0,1), normal=true standard normal (Box-Muller).
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuRandom(const Dimensions& dims, bool normal);
//=============================================================================
// Relational and logical operators (result is a logical gpuArray)
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuEq(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuNe(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLt(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLe(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuGt(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuGe(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAnd(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuOr(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuXor(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuNot(GpuArrayObject* a);
//=============================================================================
// Reductions along a dimension. dim == -1 selects the first non-singleton
// dimension (the default behaviour). Only 2-D inputs are supported.
// Result stays device resident.
//=============================================================================
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSum(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuProd(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMean(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMaxAlong(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMinAlong(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAny(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAll(GpuArrayObject* a, int dim);
//=============================================================================
// Variance and standard deviation along a dimension (single result).
// normalizeByN selects the 1/N normalization (default is the 1/(N-1) sample
// form). Integer and complex inputs raise a clear error, as on the host.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuVar(GpuArrayObject* a, int dim, bool normalizeByN);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuStd(GpuArrayObject* a, int dim, bool normalizeByN);
//=============================================================================
// Concatenate parts along an arbitrary dimension (N-D). dim 1/2 have dedicated
// 2-D paths (GpuVertcat/GpuHorzcat); this handles dim >= 3.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCatND(const std::vector<GpuArrayObject*>& parts, int dim);
//=============================================================================
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCummax(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCummin(GpuArrayObject* a, int dim);
//=============================================================================
// flip reverses along a dimension; circshift shifts (with wrap-around); repmat
// tiles an m-by-n grid of copies; diff takes n-th order differences along a
// dimension; kron is the Kronecker product; trace sums the main diagonal.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuFlip(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCircshift(GpuArrayObject* a, int shift, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuRepmat(GpuArrayObject* a, indexType rowCopies, indexType colCopies);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuDiff(GpuArrayObject* a, int order, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuKron(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuTrace(GpuArrayObject* a);
// Upper (upper==true) / lower (upper==false) triangular part above/below the
// k-th diagonal; masked on the device, keeps the class and complex layout.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuTriangular(GpuArrayObject* a, int k, bool upper);
//=============================================================================
// Prefix scans along a dimension (output has the same shape as the input).
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCumsum(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCumprod(GpuArrayObject* a, int dim);
//=============================================================================
// Sort along a dimension. Returns {sortedValues, indices} (indices 1-based),
// both device resident. `descend` selects descending order.
NLSGPU_ENGINE_IMPEXP ArrayOfVector
GpuSort(GpuArrayObject* a, int dim, bool descend);
//=============================================================================
// find: linear indices (1-based) of the non-zero elements, as a gpuArray.
// Uses a host pass over the gathered mask: find is not a natural data-parallel
// primitive. `limit` <= 0 returns all matches.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuFind(GpuArrayObject* a);
//=============================================================================
// arrayfun: run a compiled WGSL elementwise expression over N device inputs in
// a single fused kernel. `wgslExpr` references inputs as p0, p1, ... in order.
// Scalar inputs broadcast. Result stays device resident.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuArrayfun(
    const std::string& wgslExpr, bool logicalResult, const std::vector<GpuArrayObject*>& inputs);
//=============================================================================
// arrayfun for a small integer class (int8/uint8/int16/uint16): each input is
// read as an i32 and the compiled expression saturates every step to the class
// range. `className` tags the (non-logical) result. Result stays device resident.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuArrayfunInteger(const std::string& wgslExpr, bool logicalResult, const std::string& className,
    const std::vector<GpuArrayObject*>& inputs);
//=============================================================================
// max/min with a second output: {values, indices} (1-based), both device
// resident. Indices are returned as a single gpuArray.
NLSGPU_ENGINE_IMPEXP ArrayOfVector
GpuMaxAlong2(GpuArrayObject* a, int dim);
NLSGPU_ENGINE_IMPEXP ArrayOfVector
GpuMinAlong2(GpuArrayObject* a, int dim);
//=============================================================================
/**
 * Matrix multiplication (*) on the device (tiled f32 GEMM, column-major).
 * Result stays device resident. Scalar operands route to GpuTimes.
 */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMtimes(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
// Reorganization (result stays device resident; underlyingType preserved)
//=============================================================================
/** 2-D transpose. conjugate = true applies the complex conjugate ('). */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuTranspose(GpuArrayObject* a, bool conjugate = false);
//=============================================================================
/** reshape to newDims (element count must match; data copied). */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuReshape(GpuArrayObject* a, const Dimensions& newDims);
//=============================================================================
/** Linear indexing read A(idx); indices are 1-based. Output takes outDims. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuIndexLinear(
    GpuArrayObject* a, const std::vector<double>& oneBasedLinearIdx, const Dimensions& outDims);
//=============================================================================
/** 2-D indexing read A(rows, cols); subscripts are 1-based. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuIndex2D(GpuArrayObject* a, const std::vector<double>& oneBasedRows,
    const std::vector<double>& oneBasedCols);
//=============================================================================
/** N-D indexing read A(s1, s2, ..., sN); each subscript list is 1-based. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuIndexND(GpuArrayObject* a, const std::vector<std::vector<double>>& subs);
//=============================================================================
/** Flatten N one-based subscript lists to the column-major linear indices of
 *  their cartesian product; also returns the resulting shape. Shared by the N-D
 *  read and write paths. */
NLSGPU_ENGINE_IMPEXP std::vector<uint32_t>
GpuCartesianLinearIndices(
    const Dimensions& dims, const std::vector<std::vector<double>>& subs, Dimensions& outDims);
//=============================================================================
// Concatenation (result device resident; underlyingType from the first part)
//=============================================================================
/** Horizontal concatenation [A, B, ...] (equal row counts). */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuHorzcat(const std::vector<GpuArrayObject*>& parts);
//=============================================================================
/** Vertical concatenation [A; B; ...] (equal column counts). */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuVertcat(const std::vector<GpuArrayObject*>& parts);
//=============================================================================
// Indexed assignment (value semantics: returns a NEW gpuArray, input intact).
// `values` is a device array: scalar (broadcast) or matching the index count.
//=============================================================================
/** A(idx) = values; idx 1-based linear. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSubsasgnLinear(
    GpuArrayObject* a, const std::vector<double>& oneBasedIdx, GpuArrayObject* values);
//=============================================================================
/** A(rows, cols) = values; subscripts 1-based. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSubsasgn2D(GpuArrayObject* a, const std::vector<double>& oneBasedRows,
    const std::vector<double>& oneBasedCols, GpuArrayObject* values);
//=============================================================================
/** A(s1, s2, ..., sN) = values; each subscript list is 1-based. */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSubsasgnND(
    GpuArrayObject* a, const std::vector<std::vector<double>>& subs, GpuArrayObject* values);
//=============================================================================
} // namespace Nelson
//=============================================================================

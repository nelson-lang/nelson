//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <array>
#include <cmath>
#include <complex>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "IsApprox.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    inline bool
    approxValue(double a, double b, double absTol, double relTol)
    {
        if (!(std::isfinite(a) && std::isfinite(b))) {
            return a == b;
        }
        double d = std::fabs(a - b);
        return d <= absTol || d <= relTol * std::max(std::fabs(a), std::fabs(b));
    }
    //=============================================================================
    inline bool
    approxValue(
        const std::complex<double>& a, const std::complex<double>& b, double absTol, double relTol)
    {
        if (!(std::isfinite(a.real()) && std::isfinite(a.imag()) && std::isfinite(b.real())
                && std::isfinite(b.imag()))) {
            return a == b;
        }
        double d = std::abs(a - b);
        return d <= absTol || d <= relTol * std::max(std::abs(a), std::abs(b));
    }
    //=============================================================================
    // operand layout inside the expanded output
    struct Operand
    {
        indexType count = 0;
        std::array<indexType, MAXDIMS> strides {};
    };
    //=============================================================================
    struct Expansion
    {
        Dimensions dims;
        indexType nDims = 0;
        std::array<indexType, MAXDIMS> lengths {};
        std::array<Operand, 4> operands;
    };
    //=============================================================================
    Expansion
    expandDimensions(const std::array<Dimensions, 4>& inputDims)
    {
        Expansion e;
        indexType nDims = 2;
        for (const Dimensions& d : inputDims) {
            nDims = std::max(nDims, d.length());
        }
        std::vector<indexType> out(static_cast<size_t>(nDims), 1);
        for (indexType k = 0; k < nDims; ++k) {
            indexType len = 1;
            for (const Dimensions& d : inputDims) {
                indexType dk = d.dimensionLength(k);
                if (dk == 1) {
                    continue;
                }
                if (len != 1 && dk != len) {
                    Error(L"Nelson:sizeDimensionsMustMatch",
                        L"Arrays have incompatible sizes for this operation.");
                }
                len = dk;
            }
            out[static_cast<size_t>(k)] = len;
            e.lengths[k] = len;
        }
        e.nDims = nDims;
        for (size_t op = 0; op < inputDims.size(); ++op) {
            indexType stride = 1;
            for (indexType k = 0; k < nDims; ++k) {
                indexType dk = inputDims[op].dimensionLength(k);
                e.operands[op].strides[k] = dk == 1 ? 0 : stride;
                stride *= dk;
            }
            e.operands[op].count = inputDims[op].elementCount();
        }
        e.dims = Dimensions(out);
        e.dims.simplify();
        return e;
    }
    //=============================================================================
    template <class T>
    void
    compareExpanded(const T* a, const T* b, const double* absTol, const double* relTol,
        logical* out, const Expansion& e)
    {
        indexType total = e.dims.elementCount();
        const auto& ops = e.operands;
        bool flat = true;
        for (const Operand& op : ops) {
            flat = flat && (op.count == total || op.count == 1);
        }
        if (flat) {
            indexType sa = ops[0].count == 1 ? 0 : 1;
            indexType sb = ops[1].count == 1 ? 0 : 1;
            indexType st = ops[2].count == 1 ? 0 : 1;
            indexType sr = ops[3].count == 1 ? 0 : 1;
            OMP_PARALLEL_FOR_LOOP(total)
            for (ompIndexType k = 0; k < (ompIndexType)total; ++k) {
                out[k] = approxValue(a[k * sa], b[k * sb], absTol[k * st], relTol[k * sr]);
            }
            return;
        }
        // walk the output column by column, keeping one offset per operand
        std::array<indexType, MAXDIMS> coords {};
        std::array<indexType, 4> offsets {};
        indexType inner = e.lengths[0];
        for (indexType k = 0; k < total; k += inner) {
            for (indexType i = 0; i < inner; ++i) {
                out[k + i] = approxValue(a[offsets[0] + i * ops[0].strides[0]],
                    b[offsets[1] + i * ops[1].strides[0]],
                    absTol[offsets[2] + i * ops[2].strides[0]],
                    relTol[offsets[3] + i * ops[3].strides[0]]);
            }
            for (indexType d = 1; d < e.nDims; ++d) {
                coords[d]++;
                for (size_t op = 0; op < 4; ++op) {
                    offsets[op] += ops[op].strides[d];
                }
                if (coords[d] < e.lengths[d]) {
                    break;
                }
                for (size_t op = 0; op < 4; ++op) {
                    offsets[op] -= coords[d] * ops[op].strides[d];
                }
                coords[d] = 0;
            }
        }
    }
    //=============================================================================
    void
    checkIntegerRangeOf(const ArrayOf& value)
    {
        constexpr double flintmax = 9007199254740992.;
        indexType n = value.elementCount();
        bool tooLarge = false;
        if (value.dataClass() == NLS_INT64) {
            const auto* p = static_cast<const int64*>(value.dataPointer());
            for (indexType k = 0; k < n && !tooLarge; ++k) {
                tooLarge = p[k] > (int64)flintmax || p[k] < -(int64)flintmax;
            }
        } else if (value.dataClass() == NLS_UINT64) {
            const auto* p = static_cast<const uint64*>(value.dataPointer());
            for (indexType k = 0; k < n && !tooLarge; ++k) {
                tooLarge = p[k] > (uint64)flintmax;
            }
        }
        if (tooLarge) {
            Error(L"Nelson:isapprox:integerTooLarge",
                L"int64 and uint64 values above flintmax cannot be compared exactly.");
        }
    }
    //=============================================================================
    // full double (or double complex) copy only when the input is not already one
    ArrayOf
    asFullDouble(const ArrayOf& value, bool asComplex)
    {
        NelsonType target = asComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
        if (value.dataClass() == target && !value.isSparse()) {
            return value;
        }
        ArrayOf converted(value);
        if (converted.isSparse()) {
            converted.makeDense();
        }
        converted.promoteType(target);
        return converted;
    }
    //=============================================================================
}
//=============================================================================
void
IsApproxCheckIntegerRange(const ArrayOf& A, const ArrayOf& B)
{
    checkIntegerRangeOf(A);
    checkIntegerRangeOf(B);
}
//=============================================================================
double
IsApproxLevelTolerance(IsApproxLevel level, bool singlePrecision)
{
    switch (level) {
    case IsApproxLevel::VeryTight:
        return singlePrecision ? 5e-7 : 1e-15;
    case IsApproxLevel::Tight:
        return singlePrecision ? 1e-6 : 1e-12;
    case IsApproxLevel::Loose:
        return singlePrecision ? 1e-4 : 1e-8;
    case IsApproxLevel::VeryLoose:
    default:
        return singlePrecision ? 1e-2 : 1e-3;
    }
}
//=============================================================================
ArrayOf
IsApprox(const ArrayOf& A, const ArrayOf& B, const ArrayOf& absoluteTolerance,
    const ArrayOf& relativeTolerance)
{
    IsApproxCheckIntegerRange(A, B);
    Expansion e = expandDimensions({ A.dimensions(), B.dimensions(), absoluteTolerance.dimensions(),
        relativeTolerance.dimensions() });
    indexType total = e.dims.elementCount();
    logical* out = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, total));
    ArrayOf result(NLS_LOGICAL, e.dims, out);
    if (total > 0) {
        bool asComplex = A.isComplex() || B.isComplex();
        ArrayOf a = asFullDouble(A, asComplex);
        ArrayOf b = asFullDouble(B, asComplex);
        ArrayOf absTol = asFullDouble(absoluteTolerance, false);
        ArrayOf relTol = asFullDouble(relativeTolerance, false);
        const auto* pAbs = static_cast<const double*>(absTol.dataPointer());
        const auto* pRel = static_cast<const double*>(relTol.dataPointer());
        if (asComplex) {
            compareExpanded(static_cast<const std::complex<double>*>(a.dataPointer()),
                static_cast<const std::complex<double>*>(b.dataPointer()), pAbs, pRel, out, e);
        } else {
            compareExpanded(static_cast<const double*>(a.dataPointer()),
                static_cast<const double*>(b.dataPointer()), pAbs, pRel, out, e);
        }
    }
    if (A.isSparse() && B.isSparse()) {
        result.makeSparse();
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

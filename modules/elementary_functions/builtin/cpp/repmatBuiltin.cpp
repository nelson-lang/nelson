//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#define EIGEN_NO_DEBUG
#include <Eigen/Dense>
#include "repmatBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefMixinNames.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SparseArrayOf.hpp"
#include "Decomplexify.hpp"
#include <Eigen/Sparse>
#include <vector>
#include <cmath>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
ArrayOf
sparse2DRepmat(const ArrayOf& x, const Dimensions& outDims, const Dimensions& repcount)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    const auto* spmatIn = static_cast<const SparseMatrix*>(x.sparseDataPointer());
    auto* spmatOut = new SparseMatrix(outDims.rows(), outDims.columns());
    std::vector<Triplet> triplets;
    Dimensions localRepcount(repcount);
    indexType repeatCount = localRepcount[0] * localRepcount[1];
    triplets.reserve(static_cast<size_t>(spmatIn->nonZeros() * repeatCount));
    for (indexType rowBlock = 0; rowBlock < localRepcount[0]; ++rowBlock) {
        for (indexType colBlock = 0; colBlock < localRepcount[1]; ++colBlock) {
            signedIndexType rowOffset = static_cast<signedIndexType>(rowBlock * x.rows());
            signedIndexType colOffset = static_cast<signedIndexType>(colBlock * x.columns());
            for (signedIndexType col = 0; col < spmatIn->outerSize(); ++col) {
                for (typename SparseMatrix::InnerIterator it(*spmatIn, col); it; ++it) {
                    triplets.emplace_back(it.row() + rowOffset, it.col() + colOffset, it.value());
                }
            }
        }
    }
    spmatOut->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseArrayOfFromEigen<T>(x.dataClass(), outDims, spmatOut);
}
//=============================================================================
template <class T>
ArrayOf
scalarRealRepmat(const ArrayOf& x, void* dp, const Dimensions& outDims)
{
    const T* ptrValue = static_cast<const T*>(x.dataPointer());
    T value = ptrValue[0];
    T* ptrA = static_cast<T*>(dp);
    ompIndexType nbElements = (ompIndexType)outDims.elementCount();
    if (nbElements == 1) {
        ptrA[0] = value;
    } else {
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            ptrA[k] = value;
        }
    }
    return ArrayOf(x.dataClass(), outDims, dp, false);
}
//=============================================================================
template <class T>
ArrayOf
scalarComplexRepmat(const ArrayOf& x, void* dp, const Dimensions& outDims)
{
    auto* ptrXz = reinterpret_cast<std::complex<T>*>((T*)x.dataPointer());
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);

    std::complex<T> value = ptrXz[0];
    ompIndexType nbElements = (ompIndexType)outDims.elementCount();
    if (nbElements == 1) {
        ptrDpz[0] = value;
    } else {
        OMP_PARALLEL_FOR_LOOP(nbElements)
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            ptrDpz[k] = value;
        }
    }
    return ArrayOf(x.dataClass(), outDims, dp, false);
}
//=============================================================================
// Contiguous-memcpy 2-D replicate. The output is column-major, so the first N
// output columns are the input columns tiled rm times vertically, and the whole
// N-column block then repeats cn times horizontally as one large copy.
static void
repmat2D(const char* in, char* out, size_t es, indexType M, indexType N, indexType rm, indexType cn)
{
    if (M == 0 || N == 0 || rm == 0 || cn == 0) {
        // Any zero factor makes the output empty: nothing to copy (writing the
        // source columns anyway would overflow the zero-length buffer).
        return;
    }
    indexType Mo = M * rm;
    if (M == 1) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType j = 0; j < (ompIndexType)N; ++j) {
            const char* srccol = in + (size_t)j * es;
            char* dstcol = out + (size_t)j * Mo * es;
            for (indexType r = 0; r < rm; ++r) {
                std::memcpy(dstcol + (size_t)r * es, srccol, es);
            }
        }
    } else {
        size_t colBytes = (size_t)M * es;
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType j = 0; j < (ompIndexType)N; ++j) {
            const char* srccol = in + (size_t)j * colBytes;
            char* dstcol = out + (size_t)j * Mo * es;
            for (indexType r = 0; r < rm; ++r) {
                std::memcpy(dstcol + (size_t)r * colBytes, srccol, colBytes);
            }
        }
    }
    size_t blockBytes = (size_t)N * Mo * es;
    if (cn > 1 && blockBytes > 0) {
        OMP_PARALLEL_FOR_LOOP(cn)
        for (ompIndexType c = 1; c < (ompIndexType)cn; ++c) {
            std::memcpy(out + (size_t)c * blockBytes, out, blockBytes);
        }
    }
}
//=============================================================================
// A replication factor is a real finite integer value; a negative one counts
// as zero. Anything else (NaN, Inf, fraction, empty, text) is rejected: such a
// factor would size the result wrongly or overflow it.
static indexType
replicationFactor(double value)
{
    if (!std::isfinite(value) || std::floor(value) != value) {
        Error(L"Nelson:elementary_functions:replicationFactorsInteger",
            L"Replication factors must be a row vector of integers or integer scalars.");
    }
    return (value > 0) ? static_cast<indexType>(value) : 0;
}
//=============================================================================
static ArrayOf
replicationFactorsAsDouble(const ArrayOf& param)
{
    if (!(param.isNumeric() || param.dataClass() == NLS_LOGICAL) || param.isSparse()
        || param.isComplex()) {
        Error(L"Nelson:elementary_functions:replicationFactorsNumeric",
            L"Replication factors must be numeric.");
    }
    ArrayOf values(param);
    values.promoteType(NLS_DOUBLE);
    return values;
}
//=============================================================================
static Dimensions
replicationCounts(const ArrayOfVector& argIn)
{
    Dimensions repcount;
    if (argIn.size() == 2) {
        ArrayOf param2 = replicationFactorsAsDouble(argIn[1]);
        if (param2.isEmpty() || !(param2.isScalar() || param2.isRowVector())) {
            Error(L"Nelson:elementary_functions:replicationFactorsInteger",
                L"Replication factors must be a row vector of integers or integer scalars.");
        }
        if (param2.elementCount() > maxDims) {
            Error(L"Nelson:elementary_functions:tooManyDimensions", L"Too many dimensions!");
        }
        const auto* dp = static_cast<const double*>(param2.dataPointer());
        if (param2.isScalar()) {
            repcount[0] = replicationFactor(dp[0]);
            repcount[1] = repcount[0];
        } else {
            for (indexType i = 0; i < param2.elementCount(); i++) {
                repcount[i] = replicationFactor(dp[i]);
            }
        }
    } else {
        for (size_t k = 1; k < argIn.size(); ++k) {
            ArrayOf paramK = replicationFactorsAsDouble(argIn[k]);
            if (!paramK.isScalar()) {
                Error(L"Nelson:elementary_functions:replicationFactorsInteger",
                    L"Replication factors must be a row vector of integers or integer scalars.");
            }
            repcount[k - 1]
                = replicationFactor(static_cast<const double*>(paramK.dataPointer())[0]);
        }
    }
    return repcount;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::repmatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // R = repmat(A, m)
    // R = repmat(A, m, n)
    // R = repmat(A, m, n, p, ...)
    // R = repmat(A, [m n])
    // R = repmat(A, [m n p ...])
    ArrayOfVector retval;
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    Dimensions repcount;
    ArrayOf x = argIn[0];
    NelsonType classx = x.dataClass();
    bool isNotSupportedType = (classx == NLS_HANDLE || classx == NLS_GO_HANDLE);
    if (isNotSupportedType) {
        Error(L"Nelson:repmat:typeNotSupported", L"Type not supported.");
    }
    repcount = replicationCounts(argIn);
    Dimensions originalSize(x.dimensions());
    indexType outdim = (repcount.length()) > (originalSize.length()) ? (repcount.length())
                                                                     : (originalSize.length());
    Dimensions outdims;
    for (indexType i = 0; i < outdim; i++) {
        outdims[i] = originalSize[i] * repcount[i];
    }
    outdims.simplify();
    if (x.isSparse()) {
        if (!x.is2D() || !outdims.is2D() || repcount.length() > 2) {
            Error(L"Nelson:elementary_functions:NDSparseArraysSupported",
                L"ND-sparse arrays are not supported.");
        }
        switch (classx) {
        case NLS_DOUBLE:
            return sparse2DRepmat<double>(x, outdims, repcount);
        case NLS_DCOMPLEX:
            return sparse2DRepmat<doublecomplex>(x, outdims, repcount);
        case NLS_LOGICAL:
            return sparse2DRepmat<logical>(x, outdims, repcount);
        default:
            Error(L"Nelson:repmat:typeNotSupported", L"Type not supported.");
        }
    }
    void* dp = ArrayOf::allocateArrayOf(classx, outdims.elementCount(), x.fieldNames(), false);
    indexType colsize = originalSize[0];
    if (!outdims.isEmpty(false)) {
        if (x.isScalar() && classx <= NLS_CHAR) {
            switch (classx) {
            case NLS_DOUBLE: {
                return scalarRealRepmat<double>(x, dp, outdims);
            } break;
            case NLS_SINGLE: {
                return scalarRealRepmat<single>(x, dp, outdims);
            } break;
            case NLS_DCOMPLEX: {
                return scalarComplexRepmat<double>(x, dp, outdims);
            } break;
            case NLS_SCOMPLEX: {
                return scalarComplexRepmat<single>(x, dp, outdims);
            } break;
            case NLS_INT8: {
                return scalarRealRepmat<int8>(x, dp, outdims);
            } break;
            case NLS_INT16: {
                return scalarRealRepmat<int16>(x, dp, outdims);
            } break;
            case NLS_INT32: {
                return scalarRealRepmat<int32>(x, dp, outdims);
            } break;
            case NLS_INT64: {
                return scalarRealRepmat<int64>(x, dp, outdims);
            } break;
            case NLS_UINT8: {
                return scalarRealRepmat<uint8>(x, dp, outdims);
            } break;
            case NLS_UINT16: {
                return scalarRealRepmat<uint16>(x, dp, outdims);
            } break;
            case NLS_UINT32: {
                return scalarRealRepmat<uint32>(x, dp, outdims);
            } break;
            case NLS_UINT64: {
                return scalarRealRepmat<uint64>(x, dp, outdims);
            } break;
            case NLS_LOGICAL: {
                return scalarRealRepmat<logical>(x, dp, outdims);
            } break;
            case NLS_CHAR: {
                return scalarRealRepmat<charType>(x, dp, outdims);
            } break;
            default:
                break;
            }
        } else {
            if ((x.isVector() || x.is2D()) && outdims.is2D() && classx <= NLS_CHAR) {
                indexType M = x.rows();
                indexType N = x.columns();
                indexType rm = outdims.rows() / M;
                indexType cn = outdims.columns() / N;
                repmat2D(
                    (const char*)x.dataPointer(), (char*)dp, (size_t)x.elementSize(), M, N, rm, cn);
                return ArrayOf(x.dataClass(), outdims, dp, false);
            } else {
                indexType colcount = originalSize.elementCount() / colsize;
                Dimensions copySelection(outdim);
                Dimensions sourceDimensions(originalSize.length());
                indexType copyCount = repcount.elementCount();
                Dimensions anchor(outdim);
                for (indexType i = 0; i < copyCount; i++) {
                    sourceDimensions.zeroOut();
                    for (indexType j = 0; j < colcount; j++) {
                        for (ompIndexType k = 0; k < (ompIndexType)outdim; k++) {
                            anchor[k] = copySelection[k] * originalSize[k] + sourceDimensions[k];
                        }
                        x.copyElements(j * colsize, dp, outdims.mapPoint(anchor), colsize);
                        sourceDimensions.incrementModulo(originalSize, 1);
                    }
                    copySelection.incrementModulo(repcount, 0);
                }
            }
        }
    }
    if (classx == NLS_CLASSDEF_ARRAY && outdims.elementCount() != 1
        && classdefIsA(x.classType(), NLS_MIXIN_SCALAR)) {
        RaiseError(
            "'" + x.classType() + "' objects can only be scalar.", "Nelson:class:scalarArray");
    }
    ArrayOf res;
    if (classx == NLS_STRUCT_ARRAY || classx == NLS_CLASS_ARRAY || classx == NLS_CLASSDEF_ARRAY
        || classx == NLS_FUNCTION_HANDLE) {
        // field names are part of the layout: a function handle array without
        // them loses its fields (and the definition they own) on the next copy
        res = ArrayOf(classx, outdims, dp, false, x.fieldNames());
        if (classx == NLS_CLASS_ARRAY || classx == NLS_CLASSDEF_ARRAY) {
            res.setClassType(x.classType());
        }
        if (classx == NLS_CLASSDEF_ARRAY) {
            res.setClassdefLayout(x.classdefLayout());
        }
    } else {
        res = ArrayOf(classx, outdims, dp, false);
        if (outdims.isEmpty(false) && res.isComplex()) {
            // replicating into an empty array gives an empty real array
            res = decomplexify(res);
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
